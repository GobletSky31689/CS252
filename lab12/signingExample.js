var crypto = require('crypto');
var fs = require('fs');
var events = require('events');

function Signer(privKeyFile) {
  // Reads in and stores the private key
  this.privateKey = fs.readFileSync(privKeyFile).toString('ascii');
}

Signer.prototype.signMessage = function(messageFileName) {
  // Read in the file
  var msg = fs.readFileSync(messageFileName).toString('ascii');
  // Specify which signing algorithm we want to use
  var sign = crypto.createSign('RSA-SHA256');
  // Sign the message and return the signature in hexadecimal form
  return sign.update(msg).sign(this.privateKey,'hex');
}


function Verifier(pubKeyFile) {
  // Reads in and stores the public key
  this.publicKey = fs.readFileSync(pubKeyFile).toString('ascii');
}
Verifier.prototype = new events.EventEmitter();

Verifier.prototype.verifySignature = function(messageFileName, signature) {
  // Read in the file
  var msg = fs.readFileSync(messageFileName).toString('ascii');
  // Test the signature
  var legit = crypto.createVerify('RSA-SHA256').update(msg)
                   .verify(this.publicKey, signature, 'hex');
  if (legit) {
    console.log("Signature is valid");
  } else {
    console.log("Invalid signature");
  }
}

var verifier = new Verifier('./keys/charliePub.txt');
Verifier.prototype.on('signing', verifier.verifySignature);

// Valid signing
var signer = new Signer('./keys/charliePriv.txt');
var signature = signer.signMessage('./message.txt');
verifier.emit('signing', './message.txt', signature);

// Wrong signature used
signer = new Signer('./keys/bobPriv.txt');
signature = signer.signMessage('./message.txt');
verifier.emit('signing', './message.txt', signature);

