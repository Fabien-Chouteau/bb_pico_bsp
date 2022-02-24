package ILI9341.Hack is

   procedure Prepare_For_DMA (This : in out ILI9341_Device;
                              X1   : Width;
                              Y1   : Height;
                              X2   : Width;
                              Y2   : Height);

   procedure End_DMA (This : in out ILI9341_Device);

end ILI9341.Hack;
