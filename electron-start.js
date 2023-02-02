const path = require('path')
var electron = require('electron')

var app = electron.app;  // Module to control application life.
var BrowserWindow = electron.BrowserWindow;  // Module to create native browser window.

// Need this for talking to the main process, which handles the OAuth (partly).
const ipcMain = electron.ipcMain;

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

app.setAboutPanelOptions(
    {
        applicationName : "GPXmagic",
        applicationVersion : "3.10.0",
        copyright : "Peter Ward",
        version : "9bb2df0a",
        authors : "Peter Ward"
    }
);

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
  //mainWindow.openDevTools();

  // Emitted when the window is closed.
  mainWindow.on('closed', function() {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    mainWindow = null;
  });

    ipcMain.on('requestAuth', (event, config) => {

        const stravaOAuth = OAuth(config, windowParams);

        const options = config.scope;

        stravaOAuth.getAuthorizationCode(options)
            .then(code => {
                //console.log("Got token" + token)
                event.sender.send('code', code);
            }, err => {
                //console.log('Error while getting token', err);
                event.sender.send('code', null);
        });
    });

});

// Shim for OAuth module, driven by Elm code via the renderer process.
const windowParams = {
    alwaysOnTop: true,
    autoHideMenuBar: false,
    webPreferences: { nodeIntegration: false }
};


// Put OAuth stuff in here instead of as a module, so I can adjust.

const Promise = require('pinkie-promise');
const queryString = require('querystring');
const fetch = require('node-fetch');
const objectAssign = require('object-assign');
const nodeUrl = require('url');

function OAuth (config, windowParams) {

  function getAuthorizationCode(opts) {
    opts = opts || {};

    if (!config.redirectUri) {
      config.redirectUri = 'urn:ietf:wg:oauth:2.0:oob';
    }

    var urlParams = {
      response_type: 'code',
      redirect_uri: config.redirectUri,
      client_id: config.clientId
    };

    if (opts.scope) {
      urlParams.scope = opts.scope;
    }

    if (opts.accessType) {
      urlParams.access_type = opts.accessType;
    }

    var url = config.authorizationUrl + '?' + queryString.stringify(urlParams);
//    console.log(url);

    return new Promise(function (resolve, reject) {
      const authWindow = new BrowserWindow(windowParams || {'use-content-size': true});

      authWindow.loadURL(url);
      authWindow.show();

      function onCallback(url) {
        var url_parts = nodeUrl.parse(url, true);
        var query = url_parts.query;
        var code = query.code;
        var error = query.error;

        if (code) {
            authWindow.removeAllListeners('closed');
            if (url.indexOf('http://localhost') === 0) {
                // Stop when we get the auth code, Elm will get the token.
//                console.log("GOT CODE: " + code);
                resolve(code);
                setImmediate(function () {
                  authWindow.close();
                });
            }
        }
      }

      authWindow.webContents.on('will-navigate', (event, url) => {
        onCallback(url);
      });

      authWindow.webContents.on('will-redirect', (event, url) => {
//        console.log("REDIRECT" + url );
        if (url.indexOf('http://localhost') === 0)
            event.preventDefault();
        onCallback(url);
      });

      authWindow.webContents.on('did-get-redirect-request', (event, oldUrl, newUrl) => {
        onCallback(newUrl);
      });

    });
  }

  function tokenRequest(data) {
    const header = {
      'Accept': 'application/json',
      'Content-Type': 'application/x-www-form-urlencoded'
    };

    if (config.useBasicAuthorizationHeader) {
      header.Authorization = 'Basic ' + new Buffer(config.clientId + ':' + config.clientSecret).toString('base64');
    } else {
      objectAssign(data, {
        client_id: config.clientId,
        client_secret: config.clientSecret
      });
    }

    return fetch(config.tokenUrl, {
      method: 'POST',
      headers: header,
      body: queryString.stringify(data)
    }).then(res => {
//        console.log("FETCH TOKEN: " + res.json());
      return res.json();
    });
  }

  function getAccessToken(opts) {
    return getAuthorizationCode(opts)
      .then(authorizationCode => {
        var tokenRequestData = {
          code: authorizationCode,
          grant_type: 'authorization_code',
          redirect_uri: config.redirectUri
        };
        tokenRequestData = Object.assign(tokenRequestData, opts.additionalTokenRequestData);
        return tokenRequest(tokenRequestData);
      });
  }

  function refreshToken(refreshToken) {
    return tokenRequest({
      refresh_token: refreshToken,
      grant_type: 'refresh_token',
      redirect_uri: config.redirectUri
    });
  }

  return {
    getAuthorizationCode: getAuthorizationCode,
    getAccessToken: getAccessToken,
    refreshToken: refreshToken
  };
};
