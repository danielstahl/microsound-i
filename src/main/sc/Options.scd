//Set server options, run before start of server.

(
Server.local.options.blockSize = 128;
Server.internal.options.blockSize = 128;

Server.local.options.sampleRate = 96000;
Server.internal.options.sampleRate = 96000;

//Server.local.options.numInputBusChannels = 56;
//Server.internal.options.numInputBusChannels = 56;

//Server.local.options.numOutputBusChannels = 56;
//Server.internal.options.numOutputBusChannels = 56;

//Server.local.options.numAudioBusChannels = 256;
//Server.internal.options.numAudioBusChannels = 256;

Server.local.options.numBuffers = 2056;
Server.internal.options.numBuffers = 2056;

//Server.local.options.maxNodes = 2056;
//Server.internal.options.maxNodes = 2056;
)

Server.internal.options.firstPrivateBus;
Server.local.options.firstPrivateBus;


s.boot;
s.plotTree;
s.meter;
s.freqscope;
s.makeGui;
s.scope;
s.peakCPU;

//s.prepareForRecord;
//s.record;
//s.stopRecording;

// Use DiskOut to record
// See "http://doc.sccode.org/Classes/DiskOut.html"
