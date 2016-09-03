package main

import (
	"encoding/json"
	"io"
	"log"
	"net"
	"sync"

	"github.com/gopherjs/gopherjs/js"
	"github.com/mastercactapus/chrome/sockets/tcp"
	"github.com/mastercactapus/chrome/sockets/tcpserver"
	"github.com/mastercactapus/chrome/storage"
)

type Counter struct {
	mx       *sync.Mutex
	t        int
	n        int
	index    int
	canceled bool
}

func NewCounter(index int) *Counter {
	return &Counter{index: index, mx: new(sync.Mutex)}
}

func (c *Counter) Inc() {
	c.mx.Lock()
	if c.canceled {
		c.mx.Unlock()
		return
	}
	c.n++
	c.t++
	c.notify()
	c.mx.Unlock()
}
func (c *Counter) Dec() {
	c.mx.Lock()
	if c.canceled {
		c.mx.Unlock()
		return
	}
	c.n--
	c.notify()
	c.mx.Unlock()
}
func (c *Counter) notify() {
	if c.n%2 == 0 {
		countUpdate(c.index, c.n/2, c.t/2)
	}
}
func (c *Counter) Close() error {
	mx.Lock()
	c.canceled = true
	mx.Unlock()
	return nil
}

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

type appError struct {
	Type    string
	Index   int
	Message string
}
type connUpdate struct {
	Type    string
	Index   int
	Current int
	Total   int
}

func listenError(index int, message string) {
	msg := appError{Type: "listenError", Index: index, Message: message}
	js.Global.Get("chrome").Get("runtime").Call("sendMessage", msg)
}
func connectError(index int, message string) {
	msg := appError{Type: "connectError", Index: index, Message: message}
	js.Global.Get("chrome").Get("runtime").Call("sendMessage", msg)
}
func countUpdate(index, cur, total int) {
	msg := connUpdate{Type: "connectionCount", Index: index, Current: cur, Total: total}
	js.Global.Get("chrome").Get("runtime").Call("sendMessage", msg)
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

	for i, f := range cfg.Forwards {
		if !f.Enabled {
			continue
		}
		go forward(i, f.Local, f.Remote)
	}
}

func forward(index int, local, remote string) {
	l, err := tcpserver.Listen("tcp", local)
	if err != nil {
		log.Println("ERROR: listen", local, err)
		listenError(index, err.Error())
		return
	}

	registerServer(l)
	serve(index, l, remote)
}

func serve(index int, l net.Listener, remoteAddr string) {
	log.Println("SERVING:", l.Addr().String(), "->", remoteAddr)
	cnt := NewCounter(index)
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
			connectError(index, err.Error())
			c.Close()
			unregisterConn(c.(*tcp.Connection))
			continue
		}
		registerConn(nc.(*tcp.Connection))
		go pipe(c, nc, cnt)
		go pipe(nc, c, cnt)
	}
}

func pipe(a, b net.Conn, c *Counter) {
	c.Inc()
	io.Copy(a, b)
	a.Close()
	b.Close()
	unregisterConn(a.(*tcp.Connection))
	unregisterConn(b.(*tcp.Connection))
	c.Dec()
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
