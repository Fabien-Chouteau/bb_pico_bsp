with HAL;
with BBQ10KBD;

package BB_Pico_Bsp.Keyboard is

   function Key_FIFO_Pop return BBQ10KBD.Key_State;
   --  When the FIFO is empty a Key_State with Kind = Error is returned

   function Status return BBQ10KBD.KBD_Status;

   procedure Set_Backlight (Lvl : HAL.UInt8);

   function Version return HAL.UInt8;

   KEY_JOY_UP     : constant HAL.UInt8 := 16#01#;
   KEY_JOY_DOWN   : constant HAL.UInt8 := 16#02#;
   KEY_JOY_LEFT   : constant HAL.UInt8 := 16#03#;
   KEY_JOY_RIGHT  : constant HAL.UInt8 := 16#04#;
   KEY_JOY_CENTER : constant HAL.UInt8 := 16#05#;
   KEY_BTN_LEFT1  : constant HAL.UInt8 := 16#06#;
   KEY_BTN_RIGHT1 : constant HAL.UInt8 := 16#07#;
   KEY_BTN_LEFT2  : constant HAL.UInt8 := 16#11#;
   KEY_BTN_RIGHT2 : constant HAL.UInt8 := 16#12#;
end BB_Pico_Bsp.Keyboard;
