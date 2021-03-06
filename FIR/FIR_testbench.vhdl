-- Automatically generated VHDL-2002
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.FIR_types.all;

entity FIR_testbench is
  port(done : out boolean);
end;

architecture structural of FIR_testbench is
  signal finished        : boolean;
  signal system1000      : std_logic;
  signal system1000_rstn : std_logic;
  signal eta1_i1         : signed(15 downto 0);
  signal topLet_o        : signed(15 downto 0);
begin
  done <= finished;
  
  -- pragma translate_off
  process is
  begin
    system1000 <= '0';
    wait for 2 ns;
    while (not finished) loop
      system1000 <= not system1000;
      wait for 500 ns;
      system1000 <= not system1000;
      wait for 500 ns;
    end loop;
    wait;
  end process;
  -- pragma translate_on
  
  -- pragma translate_off
  system1000_rstn <= '0',
             '1' after 1 ns;
  -- pragma translate_on
  
  totest : entity FIR_topEntity_0
    port map
      (system1000      => system1000
      ,system1000_rstn => system1000_rstn
      ,eta1_i1         => eta1_i1
      ,topLet_o        => topLet_o);
  
  stimuli : entity FIR_testInput_6
    port map
      (system1000      => system1000
      ,system1000_rstn => system1000_rstn
      ,topLet_o        => eta1_i1);
  
  verify : entity FIR_expectedOutput_8
    port map
      (system1000      => system1000
      ,system1000_rstn => system1000_rstn
      ,eta_i1          => topLet_o
      ,topLet_o        => finished);
end;
