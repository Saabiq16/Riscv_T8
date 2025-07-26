/* 
see LICENSE.iitm

Author: Neel Gala
Email id: neelgala@gmail.com
Details:

-------------------------------------------------------------------------------------------------- 
*/
package replacement_icache;

import Vector::*;
`ifdef async_rst
import LFSR_Modified::*;
`else
import LFSR::*;
`endif
import Assert::*;

interface Ifc_replace#(numeric type sets, numeric type ways);
  method ActionValue#(Bit#(TLog#(ways))) line_replace(Bit#(TLog#(sets)) index, Bit#(ways) valid);
  method Action update_set(Bit#(TLog#(sets)) index, Bit#(TLog#(ways)) way);
  method Action reset_repl;
endinterface

module mkreplace#(parameter Bit#(3) alg)(Ifc_replace#(sets, ways))
  provisos(Add#(a__, TLog#(ways), 4));

  let v_ways = valueOf(ways);
  let v_sets = valueOf(sets);

  staticAssert(alg == 0 || alg == 1 || alg == 2 || alg == 3 || alg == 4, "Invalid replacement Algorithm");

  if (alg == 0) begin // RANDOM
    LFSR#(Bit#(4)) random <- mkLFSR_4();
    Reg#(Bool) rg_init <- mkReg(True);

    rule initialize_lfsr(rg_init);
      random.seed(1);
      rg_init <= False;
    endrule

    method ActionValue#(Bit#(TLog#(ways))) line_replace(Bit#(TLog#(sets)) index, Bit#(ways) valid);
      if (&valid != 1) begin
        Bit#(TLog#(ways)) temp = 0;
        for (Bit#(TAdd#(1, TLog#(ways))) i = 0; i < fromInteger(v_ways); i = i + 1) begin
          if (valid[i] == 0) temp = truncate(i);
        end
        return temp;
      end else begin
        return truncate(random.value());
      end
    endmethod

    method Action update_set(Bit#(TLog#(sets)) index, Bit#(TLog#(ways)) way) if (!rg_init);
      random.next();
    endmethod

    method Action reset_repl;
      random.seed(1);
    endmethod
  end

  else if (alg == 1) begin // RRBIN
    Vector#(sets, Reg#(Bit#(TLog#(ways)))) v_count <- replicateM(mkReg(fromInteger(v_ways - 1)));

    method ActionValue#(Bit#(TLog#(ways))) line_replace(Bit#(TLog#(sets)) index, Bit#(ways) valid);
      Bit#(TLog#(ways)) temp;
      if (&valid == 1) begin
        temp = v_count[index];
      end else begin
        temp = fromInteger(v_ways - 1);
        for (Bit#(TAdd#(1, TLog#(ways))) i = 0; i < fromInteger(v_ways); i = i + 1)
          if (valid[i] == 0) temp = truncate(i);
      end
      return temp;
    endmethod

    method Action update_set(Bit#(TLog#(sets)) index, Bit#(TLog#(ways)) way);
      v_count[index] <= v_count[index] - 1;
    endmethod

    method Action reset_repl;
      for (Integer i = 0; i < v_sets; i = i + 1)
        v_count[i] <= fromInteger(v_ways - 1);
    endmethod
  end

  else if (alg == 2) begin // PLRU
    Vector#(sets, Reg#(Bit#(TSub#(ways, 1)))) v_count <- replicateM(mkReg(5));

    method ActionValue#(Bit#(TLog#(ways))) line_replace(Bit#(TLog#(sets)) index, Bit#(ways) valid);
      if (&valid == 1) begin
        case (v_count[index]) matches
          'b?00: return 0;
          'b?10: return 1;
          'b0?1: return 2;
          default: return 3;
        endcase
      end else begin
        Bit#(TLog#(ways)) temp = 0;
        for (Bit#(TAdd#(1, TLog#(ways))) i = 0; i < fromInteger(v_ways); i = i + 1)
          if (valid[i] == 0) temp = truncate(i);
        return temp;
      end
    endmethod

    method Action update_set(Bit#(TLog#(sets)) index, Bit#(TLog#(ways)) way);
      Bit#(TSub#(ways, 1)) mask = 'b000;
      Bit#(TSub#(ways, 1)) val = 'b000;
      case (way) matches
        'd0: begin val = 'b011; mask = 'b011; end
        'd1: begin val = 'b001; mask = 'b011; end
        'd2: begin val = 'b100; mask = 'b101; end
        'd3: begin val = 'b000; mask = 'b101; end
      endcase
      v_count[index] <= (v_count[index] & ~mask) | (val & mask);
    endmethod

    method Action reset_repl;
      for (Integer i = 0; i < v_sets; i = i + 1)
        v_count[i] <= 5;
    endmethod
  end

  else if (alg == 3) begin // FIFO 
    Vector#(sets, Reg#(Bit#(TLog#(ways)))) fifo_ptrs <- replicateM(mkReg(0));

    method ActionValue#(Bit#(TLog#(ways))) line_replace(Bit#(TLog#(sets)) index, Bit#(ways) valid);
      Bit#(TLog#(ways)) selected = fifo_ptrs[index];
      Bit#(TLog#(ways)) result = selected;

      if (valid[selected] == 0) begin
        result = selected;
      end else begin
        for (Integer i = 0; i < v_ways; i = i + 1) begin
          if (valid[i] == 0) begin
            result = fromInteger(i);
          end
        end
      end

      return result;
    endmethod

    method Action update_set(Bit#(TLog#(sets)) index, Bit#(TLog#(ways)) way);
      fifo_ptrs[index] <= fifo_ptrs[index] + 1;
    endmethod

    method Action reset_repl;
      for (Integer i = 0; i < v_sets; i = i + 1)
        fifo_ptrs[i] <= 0;
    endmethod
  end

  else if (alg == 4) begin // LFU 
    Vector#(sets, Vector#(ways, Reg#(Bit#(8)))) lfu_counters <- replicateM(replicateM(mkReg(0)));

    method ActionValue#(Bit#(TLog#(ways))) line_replace(Bit#(TLog#(sets)) index, Bit#(ways) valid);
      Bit#(TLog#(ways)) min_idx = 0;
      Bit#(8) min_val = 255;
      Bool found_invalid = False;

      for (Integer i = 0; i < v_ways; i = i + 1) begin
        if (valid[i] == 0 && !found_invalid) begin
          min_idx = fromInteger(i);
          found_invalid = True;
        end
      end

      if (!found_invalid) begin
        for (Integer i = 0; i < v_ways; i = i + 1) begin
          if (lfu_counters[index][i] < min_val) begin
            min_val = lfu_counters[index][i];
            min_idx = fromInteger(i);
          end
        end
      end

      return min_idx;
    endmethod

    method Action update_set(Bit#(TLog#(sets)) index, Bit#(TLog#(ways)) way);
      lfu_counters[index][way] <= lfu_counters[index][way] + 1;
    endmethod

    method Action reset_repl;
      for (Integer i = 0; i < v_sets; i = i + 1)
        for (Integer j = 0; j < v_ways; j = j + 1)
          lfu_counters[i][j] <= 0;
    endmethod
  end

  else begin // fallback
    method ActionValue#(Bit#(TLog#(ways))) line_replace(Bit#(TLog#(sets)) index, Bit#(ways) valid);
      return ?;
    endmethod

    method Action update_set(Bit#(TLog#(sets)) index, Bit#(TLog#(ways)) way);
      noAction;
    endmethod

    method Action reset_repl;
      noAction;
    endmethod
  end
endmodule

endpackage
