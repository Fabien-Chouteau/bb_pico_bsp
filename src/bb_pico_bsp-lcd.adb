with HAL; use HAL;

with ILI9341;
with ILI9341.Hack;

with RP.GPIO;
with RP.Device;

with Pico;

with BB_Pico_Bsp.SPI;

package body BB_Pico_Bsp.LCD is

   LCD_CS    : RP.GPIO.GPIO_Point renames Pico.GP8;
   LCD_WRX   : RP.GPIO.GPIO_Point renames Pico.GP9;

   LCD_Reset : RP.GPIO.GPIO_Point renames Pico.GP11;
   --  LCD reset is not available on a GPIO on the Keyboard FeatherWing, so we
   --  take an unused GPIO to fake it.

   Device : ILI9341.ILI9341_Device (Port        => BB_Pico_Bsp.SPI.Port,
                                    Chip_Select => LCD_CS'Access,
                                    WRX         => LCD_WRX'Access,
                                    Reset       => LCD_Reset'Access,
                                    Time        => RP.Device.Timer'Access);

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      LCD_CS.Configure (RP.GPIO.Output, RP.GPIO.Pull_Up);
      LCD_WRX.Configure (RP.GPIO.Output, RP.GPIO.Pull_Up);
      LCD_Reset.Configure (RP.GPIO.Output, RP.GPIO.Pull_Up);

      Device.Initialize (ILI9341.SPI_Mode);
      Device.Set_Orientation (ILI9341.Landscape_1);
   end Initialize;

   -----------------
   -- Send_Pixels --
   -----------------

   procedure Send_Pixels (X1, Y1, X2, Y2 : Natural;
                          Addr           : System.Address;
                          Len            : HAL.UInt32;
                          Blocking       : Boolean := True)
   is
   begin
      Wait_For_DMA;

      ILI9341.Hack.Prepare_For_DMA (Device, X1, Y1, X2, Y2);

      BB_Pico_Bsp.SPI.DMA_Transmit (Addr, Len * 2);

      if Blocking then
         Wait_For_DMA;
      end if;
   end Send_Pixels;

   ------------------
   -- Wait_For_DMA --
   ------------------

   procedure Wait_For_DMA is
   begin
      loop
         exit when not BB_Pico_Bsp.SPI.DMA_Busy;
      end loop;

      --  RP.Device.Timer.Delay_Microseconds (100);

      ILI9341.Hack.End_DMA (Device);
   end Wait_For_DMA;

begin
   Initialize;
end BB_Pico_Bsp.LCD;
