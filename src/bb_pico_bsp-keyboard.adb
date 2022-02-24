with BB_Pico_Bsp.I2C;

package body BB_Pico_Bsp.Keyboard is

   Device : BBQ10KBD.BBQ10KBD_Device (BB_Pico_Bsp.I2C.Port);

   ------------------
   -- Key_FIFO_Pop --
   ------------------

   function Key_FIFO_Pop return BBQ10KBD.Key_State is
   begin
      return Device.Key_FIFO_Pop;
   end Key_FIFO_Pop;

   ------------
   -- Status --
   ------------

   function Status return BBQ10KBD.KBD_Status is
   begin
      return Device.Status;
   end Status;

   -------------------
   -- Set_Backlight --
   -------------------

   procedure Set_Backlight (Lvl : HAL.UInt8) is
   begin
      Device.Set_Backlight (Lvl);
   end Set_Backlight;

   -------------
   -- Version --
   -------------

   function Version return HAL.UInt8 is
   begin
      return Device.Version;
   end Version;

end BB_Pico_Bsp.Keyboard;
