var net = require('net');
var eol = require('os').EOL;

var srvr = net.createServer();

srvr.on('connection', function(client) {
  client.write('Hello there!' + eol);
  client.end();
});

srvr.listen(9000);

// node tcpserver.js

/*
$ telnet 127.0.0.1 9000
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Hello there!
Connection closed by foreign host.
*/

