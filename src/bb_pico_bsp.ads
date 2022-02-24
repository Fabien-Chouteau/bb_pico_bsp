private with RP.DMA;

package BB_Pico_Bsp is
   pragma Elaborate_Body;

private
   SPI_TX_DMA : constant RP.DMA.DMA_Channel_Id := 1;
end BB_Pico_Bsp;
