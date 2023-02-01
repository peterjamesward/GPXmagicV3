const path = require('path')
var electron = require('electron')

var app = electron.app;  // Module to control application life.
var BrowserWindow = electron.BrowserWindow;  // Module to create native browser window.

const ipcMain = electron.ipcMain;
const electronOauth2 = require('electron-oauth2');

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is GCed.
var mainWindow = null;

// Quit when all windows are closed.
app.on('window-all-closed', function() {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  //if (process.platform != 'darwin')
  {
    app.quit();
  }
});

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
app.on('ready', function() {
  // Create the browser window.
  mainWindow = new BrowserWindow(
  {width: 1000,
   height: 800,
   webPreferences: { preload: path.join(__dirname, 'preload.js') }
  });

  // and load the index.html of the app.
  mainWindow.loadURL('file://' + __dirname + '/site/index.html');

  // Open the devtools.
  mainWindow.openDevTools();

  // Emitted when the window is closed.
  mainWindow.on('closed', function() {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    mainWindow = null;
  });

    ipcMain.on('requestAuth', (event, config) => {

        console.log(config);
        console.log("Trying Oauth...")
        const stravaOAuth = electronOauth2(config, windowParams);

        stravaOAuth.getAccessToken({})
            .then(token => {
                console.log("Got token" + token)
                event.sender.send('token', token);
            }, err => {
                console.log('Error while getting token', err);
        });
    });

});

// Shim for OAuth module, driven by Elm code via the renderer process.
const windowParams = {
    alwaysOnTop: true,
    autoHideMenuBar: true,
    webPreferences: { nodeIntegration: false}
};

