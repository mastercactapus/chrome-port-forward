chrome.app.runtime.onLaunched.addListener(()=>{
  chrome.app.window.create("index.html",{
    id: "cfg",
    innerBounds: {
      width:400,
      height: 350
    }
  })
})
