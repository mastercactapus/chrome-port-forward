
const app = Elm.Main.fullscreen()

app.ports.saveConfig.subscribe(
	config=>chrome.storage.local.set({config})
)
app.ports.loadConfig.subscribe(
	()=>chrome.storage.local.get("config",
		items=>app.ports.loadedConfig.send(items.config||"")
	)
)

chrome.runtime.onMessage.addListener(message=>{
	switch (message.Type) {
		case "listenError":
			app.ports.listenError.send([message.Index, message.Message])
			break;
		case "connectError":
			app.ports.connectError.send([message.Index, message.Message])
			break;
		case "connectionCount":
			app.ports.countUpdate.send([message.Index, message.Current, message.Total])
			break;
	}
})
