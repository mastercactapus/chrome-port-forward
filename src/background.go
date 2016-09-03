package main

import (
	"encoding/json"
	"io"
	"log"
	"net"
	"sync"

	"github.com/mastercactapus/chrome/sockets/tcp"
	"github.com/mastercactapus/chrome/sockets/tcpserver"
	"github.com/mastercactapus/chrome/storage"
)

type Config struct {
	Forwards []struct {
		Enabled bool
		Local   string
		Remote  string
	}
}

var mx = new(sync.Mutex)
var servers = make([]*tcpserver.Server, 0, 100)
var conns = make([]*tcp.Connection, 0, 1000)

func registerServer(s *tcpserver.Server) {
	mx.Lock()
	servers = append(servers, s)
	mx.Unlock()
}
func registerConn(c *tcp.Connection) {
	mx.Lock()
	conns = append(conns, c)
	mx.Unlock()
}
func unregisterConn(c *tcp.Connection) {
	mx.Lock()
	newConns := conns[:0]
	for _, cn := range conns {
		if cn == c {
			continue
		}
		newConns = append(newConns, cn)
	}
	conns = newConns
	mx.Unlock()
}

func resetTCPServers() {
	for _, s := range servers {
		s.Close()
	}
	servers = servers[:0]
	sck, err := tcpserver.GetSockets()
	if err != nil {
		return
	}
	for _, s := range sck {
		tcpserver.NewServer(s.SocketID).Close()
	}
}
func resetTCPConns() {
	for _, c := range conns {
		c.Close()
	}
	conns = conns[:0]
	sck, err := tcp.GetSockets()
	if err != nil {
		return
	}
	for _, s := range sck {
		tcp.NewConnection(s.SocketID).Close()
	}
}

func reset() {
	log.Println("RESET")
	resetTCPServers()
	resetTCPConns()

	cfgStr, _ := storage.Local.Get("config")

	var cfg Config
	err := json.Unmarshal([]byte(cfgStr), &cfg)
	if err != nil {
		return
	}

	for _, f := range cfg.Forwards {
		if !f.Enabled {
			continue
		}
		go forward(f.Local, f.Remote)
	}
}

func forward(local, remote string) {
	l, err := tcpserver.Listen("tcp", local)
	if err != nil {
		log.Println("ERROR: listen", local, err)
		return
	}

	registerServer(l)
	serve(l, remote)
}

func serve(l net.Listener, remoteAddr string) {
	log.Println("SERVING:", l.Addr().String(), "->", remoteAddr)
	for {
		c, err := l.Accept()
		if err != nil {
			// probably a reset, just ignore and cleanup
			l.Close()
			return
		}
		registerConn(c.(*tcp.Connection))
		log.Println("NEW CONN:", c.RemoteAddr().String())
		nc, err := tcp.Dial("tcp", remoteAddr)
		if err != nil {
			log.Println("ERROR:", err)
			c.Close()
			unregisterConn(c.(*tcp.Connection))
			continue
		}
		registerConn(nc.(*tcp.Connection))
		go pipe(c, nc)
		go pipe(nc, c)
	}
}

func pipe(a, b net.Conn) {
	io.Copy(a, b)
	a.Close()
	b.Close()
	unregisterConn(a.(*tcp.Connection))
	unregisterConn(b.(*tcp.Connection))
}

func watch() {
	l := storage.Local.NewListener()
	reset()
	for range l.C {
		reset()
	}
}

func main() {
	go watch()
}
