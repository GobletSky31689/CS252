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
    console.log(strData);
    if(strData === '\\list'){

      client.write("List of Users:\n");
      client.write(clientList.map(function(cl){
          return cl.name;
      }).join(", "));
      client.write("\n");

    } else if (strData.startsWith('\\rename')) {
       
       var start = strData.indexOf('<')+1;
       var end = strData.indexOf('>');
       if(start<=0 || end <=0){
          client.write("Invalid Command!\n");
       } else {
          var msg = client.name + ' is now ';
          client.name = strData.substring(start, end);
          msg += client.name + '\n';
          for (var i in clientList) {
             clientList[i].write(msg);
          }
       }
       
    }
    else if(strData.startsWith('\\private')) {
        tokens = strData.split("<");
        if(tokens.length !== 3 || !tokens[1].trim().endsWith('>') || !tokens[2].trim().endsWith('>')) {
           client.write("Invalid Command!\n");
        } else {
          rec_name = tokens[1].substring(0, tokens[1].length-2);
          p_msg = tokens[2].substring(0, tokens[2].length-2)+"\n";
          console.log(p_msg + " " + rec_name);
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


