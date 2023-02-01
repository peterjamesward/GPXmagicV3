// This piece of JS runs when the window is opened, and allows us to get hold of Electron context.

//TODO: Make the send function here available to gpxmagic.js.

const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
    requestAuth: (title) => ipcRenderer.send('requestAuth', title)
});
