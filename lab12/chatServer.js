var net = require('net');
var eol = require('os').EOL;

var srvr = net.createServer();
var clientList = [];

srvr.on('connection', function(client) {
  client.name = client.remoteAddress + ':' + client.remotePort;
  client.write('Welcome, ' + client.name + eol);
  clientList.push(client);

  client.on('data', function(data) {
    var strData = data.toString('utf8').trim();
    if(strData === '\\list'){

      client.write("List of Users:\n");
      client.write(clientList.map(function(cl){
          return cl.name;
      }).join(", "));
      client.write("\n");

    } else if (strData.startsWith('\\rename')) {
       
       var start = strData.indexOf(' ')+1;
       if(start<=0){
          client.write("Invalid Command!\n");
       } else {
          var msg = client.name + ' is now ';
          client.name = strData.substring(start).trim();
          msg += client.name + '\n';
          for (var i in clientList) {
             clientList[i].write(msg);
          }
       }
       
    }
    else if(strData.startsWith('\\private')) {
        startName = strData.indexOf(' ')+1;
        startMsg = strData.indexOf(' ', startName)+1;
        if(start<=0 || startMsg <=0) {
           client.write("Invalid Command!\n");
        } else {
          rec_name = strData.substring(startName, startMsg).trim();
          p_msg = strData.substring(startMsg).trim()+"\n";
          private_msg(p_msg, rec_name, client.name);
        }
    }
    else {
      broadcast(data, client);
    }
  });
});


function private_msg(data, receipName, senderName){
  for (var i in clientList) {
    if (receipName === clientList[i].name) {
       clientList[i].write(senderName + " whispers " + data);
    }
  }
}

function broadcast(data, client) {
  for (var i in clientList) {
    if (client !== clientList[i]) {
      clientList[i].write(client.name + " says " + data);
    }
  }
}

srvr.listen(9000);


