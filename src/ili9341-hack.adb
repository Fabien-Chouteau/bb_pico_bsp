with ILI9341_Regs;

package body ILI9341.Hack is

   ---------------------
   -- Prepare_For_DMA --
   ---------------------

   procedure Prepare_For_DMA  (This : in out ILI9341_Device;
                               X1   : Width;
                               Y1   : Height;
                               X2   : Width;
                               Y2   : Height)
   is
   begin
      This.Set_Cursor_Position (X1, Y1, X2, Y2);

      This.Send_Command (ILI9341_Regs.ILI9341_GRAM);

      This.WRX.Set;
      This.Chip_Select_Low;
   end Prepare_For_DMA;

   -------------
   -- End_DMA --
   -------------

   procedure End_DMA (This : in out ILI9341_Device) is
   begin
      This.Chip_Select_High;
   end End_DMA;

end ILI9341.Hack;
