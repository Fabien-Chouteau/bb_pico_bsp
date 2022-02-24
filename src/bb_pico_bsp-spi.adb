with RP.SPI; use RP.SPI;
with RP.Device;
with RP.GPIO;

with Pico;

package body BB_Pico_Bsp.SPI is

   SPI_Port : RP.SPI.SPI_Port renames RP.Device.SPI_0;

   DMA_TX_Trigger : constant RP.DMA.DMA_Request_Trigger := RP.DMA.SPI0_TX;

   SPI_CLK  : RP.GPIO.GPIO_Point renames Pico.GP18;
   SPI_DO   : RP.GPIO.GPIO_Point renames Pico.GP19;
   SPI_DI   : RP.GPIO.GPIO_Point renames Pico.GP16;

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Config : RP.SPI.SPI_Configuration;
   begin
      SPI_CLK.Configure (RP.GPIO.Output, RP.GPIO.Pull_Up, RP.GPIO.SPI);
      SPI_DO.Configure (RP.GPIO.Output, RP.GPIO.Pull_Up, RP.GPIO.SPI);
      SPI_DI.Configure (RP.GPIO.Output, RP.GPIO.Pull_Up, RP.GPIO.SPI);

      Config := RP.SPI.Default_SPI_Configuration;

      Config.Baud := 10_000_000;
      Config.Blocking := True;

      SPI_Port.Configure (Config);

      -- DMA --
      declare
         use RP.DMA;
         Config : DMA_Configuration;
      begin
         Config.Trigger := DMA_TX_Trigger;
         Config.High_Priority := True;
         Config.Data_Size := Transfer_8;
         Config.Increment_Read := True;
         Config.Increment_Write := False;

         RP.DMA.Configure (SPI_TX_DMA, Config);
      end;
   end Initialize;

   -------------
   -- Go_Slow --
   -------------

   procedure Go_Slow is
   begin
      SPI_Port.Set_Speed (1_000_000);
   end Go_Slow;

   -------------
   -- Go_Fast --
   -------------

   procedure Go_Fast is
   begin
      SPI_Port.Set_Speed (50_000_000);
   end Go_Fast;

   ----------
   -- Port --
   ----------

   function Port return not null HAL.SPI.Any_SPI_Port is
   begin
      return SPI_Port'Access;
   end Port;

   ------------------
   -- DMA_Transmit --
   ------------------

   procedure DMA_Transmit (Addr : System.Address; Len : HAL.UInt32) is
   begin
      loop
         exit when not RP.DMA.Busy (SPI_TX_DMA);
         --  Previous DMA transfer still in progress
      end loop;

      RP.DMA.Start (Channel => SPI_TX_DMA,
                    From    => Addr,
                    To      => SPI_Port.FIFO_Address,
                    Count   => Len);
   end DMA_Transmit;

   --------------
   -- DMA_Busy --
   --------------

   function DMA_Busy return Boolean
   is (RP.DMA.Busy (SPI_TX_DMA) or else SPI_Port.Transmit_Status = Busy);

begin
   Initialize;
end BB_Pico_Bsp.SPI;
