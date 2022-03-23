<CsoundSynthesizer>

<CsOptions>

--nodisplays --output=dac

</CsOptions>

<CsInstruments>

sr = 44100
ksmps = 64
nchnls = 1
0dbfs = 1.0
gkrggBpmVar init 110.0
girgfree_vco = 101
ir13 = girgfree_vco
ir15 vco2init 16, ir13
girgfree_vco = ir15
giPort init 1
opcode FreePort, i, 0
xout giPort
giPort = giPort + 1
endop




instr 21

endin

instr 20
 event_i "i", 19, 604800.0, 1.0e-2
endin

instr 19
ir1 = 18
ir2 = 0.0
 turnoff2 ir1, ir2, ir2
 exitnow 
endin

instr 18
arl0 init 0.0
ir3 = 1.0
ar0 upsamp k(ir3)
ir4 = 220.0
kr0 vco2ft ir4, 4
ar1 oscilikt ir3, ir4, kr0
ir7 = 0.0
ir8 = 90.0
ir9 = 100.0
ar2 compress ar1, ar0, ir7, ir8, ir8, ir9, ir7, ir7, 0.0
ar0 = (ar2 * 0.8)
arl0 = ar0
ar0 = arl0
 out ar0
endin

</CsInstruments>

<CsScore>



f0 604800.0

i 21 0.0 -1.0 
i 20 0.0 -1.0 
i 18 0.0 -1.0 

</CsScore>



</CsoundSynthesizer>