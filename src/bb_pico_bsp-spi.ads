with System;
with HAL;
with HAL.SPI;

package BB_Pico_Bsp.SPI is

   function Port return not null HAL.SPI.Any_SPI_Port;

   procedure Go_Slow;
   --  Set a slow baud rate for the STMPE811 touch controller

   procedure Go_Fast;
   --  Set a fast baud rate for the LCD pixel transfer

   procedure DMA_Transmit (Addr : System.Address; Len : HAL.UInt32);

   function DMA_Busy return Boolean;

end BB_Pico_Bsp.SPI;
