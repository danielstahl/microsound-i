//Set server options, run before start of server.

(
Server.local.options.blockSize = 128;
Server.internal.options.blockSize = 128;

Server.local.options.sampleRate = 96000;
Server.internal.options.sampleRate = 96000;

Server.local.options.numBuffers = 2056;
Server.internal.options.numBuffers = 2056;
)

s.boot;
s.plotTree;
s.meter;
s.freqscope;
s.makeGui;
s.scope;
s.peakCPU;
s.dumpOSC;

s.recordNode;
s.prepareForRecord;
s.record;
s.stopRecording;