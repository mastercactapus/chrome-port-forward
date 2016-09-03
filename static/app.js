
const app = Elm.Main.fullscreen()

app.ports.saveConfig.subscribe(
	config=>chrome.storage.local.set({config})
)
app.ports.loadConfig.subscribe(
	()=>chrome.storage.local.get("config",
		items=>app.ports.loadedConfig.send(items.config||"")
	)
)
