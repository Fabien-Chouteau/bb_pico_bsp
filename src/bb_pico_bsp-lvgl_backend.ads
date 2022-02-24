with Lv.Hal.Indev;

package BB_Pico_Bsp.LVGL_Backend is

   procedure Initialize (Enable_Pointer : Boolean := True);

   function Keypad_Indev return Lv.Hal.Indev.Indev_T;

end BB_Pico_Bsp.LVGL_Backend;
