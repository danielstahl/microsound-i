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


//Turn on dumpOSC
s.dumpOSC(1);

//Turn of dumpOSC
s.dumpOSC(0);


// Record

s.prepareForRecord('/Users/danielstahl/Documents/Projects/microsound-i_mix/microsound-i_sc.aiff');
s.record;
s.stopRecording;