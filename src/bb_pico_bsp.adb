with RP.Device;
with RP.Clock;
with RP.GPIO;

with Pico;

package body BB_Pico_Bsp is

begin
   RP.Clock.Initialize (Pico.XOSC_Frequency);
   RP.Clock.Enable (RP.Clock.PERI);
   RP.Device.Timer.Enable;
   RP.GPIO.Enable;
   RP.DMA.Enable;
end BB_Pico_Bsp;
