with System;

with lvgl_ada_Config;
with HAL;

package BB_Pico_Bsp.LCD is

   Width  : constant := lvgl_ada_Config.Horizontal_Resolution;
   Height : constant := lvgl_ada_Config.Vertical_Resolution;

   procedure Send_Pixels (X1, Y1, X2, Y2 : Natural;
                          Addr           : System.Address;
                          Len            : HAL.UInt32;
                          Blocking       : Boolean := True);

   procedure Wait_For_DMA;

end BB_Pico_Bsp.LCD;
