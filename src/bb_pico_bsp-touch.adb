with BB_Pico_Bsp.STMPE811;
with BB_Pico_Bsp.SPI;
with BB_Pico_Bsp.LCD;

with RP.Device;
with RP.GPIO;

with Pico;

package body BB_Pico_Bsp.Touch is

   Touch_CS : RP.GPIO.GPIO_Point renames Pico.GP7;

   Device : BB_Pico_Bsp.STMPE811.STMPE811_Device
     (Port        => BB_Pico_Bsp.SPI.Port,
      Chip_Select => Touch_CS'Access,
      Time        => RP.Device.Timer'Access);

   --------------------------
   -- Get_All_Touch_Points --
   --------------------------

   function Get_All_Touch_Points return HAL.Touch_Panel.TP_State is
   begin
      LCD.Wait_For_DMA;

      BB_Pico_Bsp.SPI.Go_Slow;

      declare
         Result : constant HAL.Touch_Panel.TP_State :=
           Device.Get_All_Touch_Points;
      begin
         BB_Pico_Bsp.SPI.Go_Fast;
         return Result;
      end;
   end Get_All_Touch_Points;

begin
   Touch_CS.Configure (RP.GPIO.Output, RP.GPIO.Pull_Up);
   Touch_CS.Set;

   LCD.Wait_For_DMA;

   RP.Device.Timer.Delay_Milliseconds (1);

   BB_Pico_Bsp.SPI.Go_Slow;

   if not Device.Initialize then
      raise Program_Error;
   end if;

   Device.Set_Bounds (Width  => BB_Pico_Bsp.LCD.Height,
                      Height => BB_Pico_Bsp.LCD.Width,
                      Swap   => HAL.Touch_Panel.Swap_XY);

   BB_Pico_Bsp.SPI.Go_Fast;

end BB_Pico_Bsp.Touch;
