const bitcoin = require("bitcoinjs-lib");
const bip32 = require('bip32');

const bitcoin_network = bitcoin.bitcoin;

const ffiHash256Hex = function (hex) {
    return bitcoin.crypto.hash256(Buffer.from(hex, 'utf8')).toString('hex');
};

const ffiHash160Hex = function (hex) {
    return bitcoin.crypto.hash160(Buffer.from(hex, 'utf8')).toString('hex');
};

const ffiMkBip32 = function (xPub) {
  return bip32.fromBase58(xPub, bitcoin_network);
};

const ffiDerivePath = function (hdNode) {
  return function (path){
    return hdNode.derivePath(path);
  };
};

const ffiGetAddress = function (node) {
  return bitcoin.payments.p2pkh(
    { pubkey: node.publicKey, network: bitcoin_network }
  ).address;
};

exports.ffiHash256Hex = ffiHash256Hex;
exports.ffiHash160Hex = ffiHash160Hex;
exports.ffiMkBip32 = ffiMkBip32;
exports.ffiDerivePath = ffiDerivePath;
exports.ffiGetAddress = ffiGetAddress;
