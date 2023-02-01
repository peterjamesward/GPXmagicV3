// This piece of JS runs when the window is opened, and allows us to get hold of Electron context.

//TODO: Make the send function here available to gpxmagic.js.

const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
    requestAuth: (config) => ipcRenderer.send('requestAuth', config)
});

ipcRenderer.on('token', (_event, token) => {
    console.log("BACK IS THE TOKEN NOW: " + token);
})
