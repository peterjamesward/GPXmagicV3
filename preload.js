// This piece of JS runs when the window is opened, and allows us to get hold of Electron context.

const { contextBridge, ipcRenderer } = require('electron')

var responseFn;

contextBridge.exposeInMainWorld('electronAPI', {
    setResponseFn: (f) => responseFn = f,
    requestAuth: (config) => ipcRenderer.send('requestAuth', config)
});


ipcRenderer.on('code', (_event, code) => {
    responseFn({ Cmd : 'Code', code : code});
})
