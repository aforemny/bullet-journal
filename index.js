var express = require('express');
var ParseServer = require('parse-server').ParseServer;
var app = express();

var api = new ParseServer({
  databaseURI: 'mongodb://localhost:27017/dev',
  cloud: './cloud/main.js',
  appId: 'bujo',
  masterKey: 'bujo',
  serverURL: 'http://localhost:1337/parse'
});

app.use('/parse', api);

app.get('/fonts/roboto/:file', function(req, res) {
  res.sendFile(req.params.file, { root: __dirname + '/_site/fonts/roboto/' });
});

app.get('/:file', function(req, res) {
  res.sendFile(req.params.file, { root: __dirname + '/_site/' });
});

app.get('/', function(req, res) {
  res.sendFile('index.html', { root: __dirname + '/_site/' });
});

app.listen(1337, function() {
  console.log('parse-server-example running on port 1337.');
});
