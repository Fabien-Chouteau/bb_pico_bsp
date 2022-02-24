with RP.Device;
with RP.I2C_Master;
with RP.GPIO;

with Pico;

package body BB_Pico_Bsp.I2C is

   I2C_Port : RP.I2C_Master.I2C_Master_Port renames RP.Device.I2C_0;
   I2C_SDA  : RP.GPIO.GPIO_Point renames Pico.GP4;
   I2C_SCL  : RP.GPIO.GPIO_Point renames Pico.GP5;

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      I2C_SDA.Configure (RP.GPIO.Output, RP.GPIO.Pull_Up, RP.GPIO.I2C);
      I2C_SCL.Configure (RP.GPIO.Output, RP.GPIO.Pull_Up, RP.GPIO.I2C);

      I2C_Port.Configure (400_000);
   end Initialize;

   ----------
   -- Port --
   ----------

   function Port return not null HAL.I2C.Any_I2C_Port is
   begin
      return I2C_Port'Access;
   end Port;

begin
   Initialize;
end BB_Pico_Bsp.I2C;
