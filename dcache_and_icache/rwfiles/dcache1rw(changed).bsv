/* 
see LICENSE.iitm

Author: Neel Gala
Email id: neelgala@gmail.com
Details:

Working Principle
-----------------

A request from the core is enqueued into a request fifo (``ff_core_request``). On a hit within the 
cache, the required word is enqueued into the response fifo (``ff_core_response``) which is read by 
the core. On a miss, a read request for the line is sent to the fabric via the 
``ff_mem_rd_request`` and simultaneously an entry in the fill-buffer is allotted to capture the 
fabric response. The responses from the fabric are enqueued in the ``ff_mem_rd_resp`` fifo. 
When a dirty line needs to be evicted, a write request for that line is enqueued into 
``ff_mem_wr_request`` fifo and the response of this write is captured in ``ff_mem_wr_resp`` 
fifo.

Serving core requests
^^^^^^^^^^^^^^^^^^^^^

A core request can only be enqueued in ``ff_core_request`` fifo if the following conditions are true :- 

1. Fill-buffer is not full.
2. Core is ready to receive a response or deq the previous response
3. Fence operation is not in progress.
4. A replay of SRAM tag and data request (for a previous request) is not happening 
   (its necessity is discussed in later sections).

The reason for point 1 and 2 being, once either of the two structures are full, a hit or a miss 
cannot be processed further. In this situation, if there is one outstanding request already 
present in ``ff_core_request``, enqueuing one more request would overwrite the SRAM tag and data 
values of the previous one. When tag matching resumes, incorrect tag would be used leading to 
incorrect behaviour.

Once a request is enqueued into the ``ff_core_request`` fifo, a tag and data read request is sent to 
the SRAMs simultaneously. In the next cycle, if there isn't a pending request and fill-buffer & 
ff_core_response are not full, the tag field of the request is compared with the tags stored in 
the SRAMs (tag field of all the ways for particular set) and the fill-buffer 
(tag field of all the entries). 

A hit occurs in following scenarios :- 1. Tag matches in SRAM 2. Tag matches in fill-buffer and 
also the requested word is present. There might be a case where tag matches in fill-buffer but the 
word is not present as the line is still getting filled by the fabric. In that case we keep 
polling on the fill-buffer until there's a **word-hit**. Please note, that a tag-hit can occur 
either in the SRAM or the fill-buffer and never both. Assertions to check this have been put in 
place. A miss occurs when tag match fails in both the SRAM and the fill-buffer. Now following 
scenarios can occur :-

1. **For a Load request**: if it's a hit, the requested word is enqueued in ``ff_core_response`` 
   fifo in the same cycle as the tag-match. When it's a hit in the FB, before enqueuing the response, 
   we check if there is a pending store to the same word, if so we enqueue the updated word 
   accordingly. Since, the SRAMs are not updated with stores immediately, the store-buffer is 
   looked up only in the case of a fill-buffer hit.

2. **For a Load request**: If it's a miss, the address (after making it word aligned) is 
   enqueued into the ``ff_mem_rd_request`` fifo to be sent to fabric. Simultaneously, a 
   fill-buffer entry is assigned to capture the line requested from the fabric. Once the 
   requested word is captured in the fill-buffer (while rest of the line is still getting filled), 
   it is enqueued into the ``ff_core_response`` to be sent to core and the entry in ``ff_core_request`` 
   is dequeued. We are now ready to service the subsequent request in the next cycle.

3. **For a store request**: If it's a hit in the fill-buffer, a store buffer entry is allotted to 
   store the data to be written and response is enqueued in the ``ff_core_response`` fifo 
   (response being that it is store hit). If it's a hit in the SRAM, in addition to performing 
   actions that of a fill-buffer hit, the line is copied into the fill-buffer (since all stores 
   are performed here) while making it invalid in the SRAM.

4. **For a store request**: If it's a miss, request would be sent to fabric as was when 
   load miss occurred. Once the requested word is captured in the fill-buffer, the actions that 
   follow are similar to those of store hit in fill-buffer.

5. **For atomic requests**: The control is similar to that store-requests apart from the fact 
   that the updated word undergoes arithmetic op before being written in the store-buffer.

Release from fill-buffer
^^^^^^^^^^^^^^^^^^^^^^^^

The necessary condition for a release of a line from fill-buffer and its updation into SRAM is 
that the line itself is valid and all the words in the line are present and updated by store-buffer 
if necessary. If there is any pending store in the store buffer, the line won't be released. 
Given this is true, following conditions would initiate a release :- 

1. **Fill-buffer is full**. A release is necessary in this case since no more requests can be 
   taken and it can stall the pipe. While the release happens, suppose there is an entry already 
   present in the request fifo which is to the line being released. The tag and data for that 
   entry have already been read and would be used to check hit/miss. The SRAM tag matching would 
   take place with a stale value and would result in a miss. It would also be a miss in fill-buffer 
   since the line would already have been released. To prevent this incorrect behaviour, we need to 
   replay the SRAM tag and data requests (now it would be a hit in SRAM).

2. **Opportunistic fill**: if the fill-buffer is not full but there is no request being enqueued 
   in a particular cycle (this does not mean ``ff_core_request`` is empty). Given this, if there is 
   an entry in ``ff_core_request`` to the line being released, we prevent the release for not 
   wanting to replay the SRAM read request (described in point 1).

Now given the release can actually take place, following scenarios would arise :-

1. If the line in the SRAM being evicted is not dirty, then we can directly put a write request 
   (of the line being released) to the SRAM along with updation of the SRAM dirty and valid bits 
   accordingly.
2. If the line being replaced is dirty, we need to write it back to fabric. So first we put a read 
   request to SRAM for the dirty line, in the next cycle we enqueue this line in the 
   ``ff_mem_wr_request`` for it to be written back in fabric while also putting a SRAM write 
   request for line being released.

Once a release is done from the fill-buffer, that particular entry in the fill-buffer is 
invalidated and thus is available for new allocation on a miss or a store-hit.

The fill-buffer is implemented as a circular-buffer with head and tail pointer-registers.

Fence operation
^^^^^^^^^^^^^^^

A cache-flush operation is initiated when the core presents a fence instruction. A fence operation 
can only start if following conditions are met:

1. the entire fill-buffer is empty (i.e. all lines are updated in the SRAM).
2. there are not pending write-backs to fabric 
3. the store-buffer is empty.

In case of the D-Cache, the fence operation is a single cycle operation if the global-dirty bit 
is clear, where all the lines are invalidated and the dirty bits of each line are cleared as well. 
If the global-dirty bit is set, the fence operation in the D-Cache traverses through each set and 
identifies which lines need to the written back to the fabric. Traversing a set, requires 
traversing each of the way and checking if a write-back is required. A set is ignored 
if there are no valid dirty lines in the set. At the end of each set traversal, the valid and 
dirty bits of the entire set are cleared. The fence operation in the D-Cache is only over when the 
last set has been completely traversed. Until this point, not new requests are entertained from the 
core-side.

--------------------------------------------------------------------------------------------------
*/
package dcache1rw;
  `include "Logger.bsv"
  import FIFO :: * ;
  import FIFOF :: * ;
`ifdef async_rst
import SpecialFIFOs_Modified :: * ;
`else
import SpecialFIFOs :: * ;
`endif  
  import BRAMCore :: * ;
  import Vector :: * ;
  import GetPut :: * ;
  import Assert  :: * ;
  import OInt :: * ;
  import BUtils :: * ;
  import Memory :: * ; // only for the updateDataWithMask function
  import DReg :: * ;
  import UniqueWrappers :: * ;


  `include "dcache.defines"
  import dcache_types :: * ;
  import dcache_lib :: * ;
  import replacement_dcache :: * ;
`ifdef supervisor
  import common_tlb_types:: * ;
`endif
`ifdef dcache_ecc
  import ecc_hamming :: * ;
`endif
`ifdef pmp
  import pmp_func :: *;
`endif


  import io_func :: * ;
 
  typedef struct{
    Bit#(TMax#(1,TLog#(blocks))) init_bank;
    Bit#(TLog#(fbsize)) fbindex;
  } Pending_req#(numeric type fbsize, numeric type blocks) deriving(Bits, Eq);

  instance FShow#(Pending_req#(fbszie, blocks));
    /*doc:func: */
    function Fmt fshow (Pending_req#(fbsize, blocks) value);
      Fmt result = $format("{fbindex:%d bank:%b}",value.fbindex, value.init_bank);
      return result;      
    endfunction
  endinstance


  interface Ifc_dcache;
    // -- core side interfaces
    interface Put#(DCache_core_request#(`vaddr,TMul#(`dwords,8),`desize)) receive_core_req;
    interface Get#(DMem_core_response#(TMul#(`dwords,8),`desize))         send_core_cache_resp;
    method Maybe#(DMem_core_response#(TMul#(`dwords,8),`desize))          send_core_io_resp;

    // -- memory side cache interfaces
    interface Get#(DCache_mem_readreq#(`paddr)) send_mem_rd_req;
    interface Put#(DCache_mem_readresp#(`dbuswidth)) receive_mem_rd_resp;
    method DCache_mem_writereq#(`paddr, TMul#(`dblocks, TMul#(`dwords, 8))) send_mem_wr_req;
    method Action deq_mem_wr_req;
    interface Put#(DCache_mem_writeresp) receive_mem_wr_resp;

    // -- memory side io interfaces
    interface Get#(DCache_io_req#(`paddr, `dbuswidth)) send_mem_io_req;
    interface Put#(DCache_io_response#(`dbuswidth)) receive_mem_io_resp;

    // -- csr side-bands required
    method Action ma_curr_priv (Bit#(2) c);
    method Action ma_cache_enable(Bool c);

    // -- status of cache
    method Bool mv_storebuffer_empty;
    method Bool mv_cache_available;

    // commits to be indicated by the pipeline
    //method Action ma_commit_store(Bit#(`desize) currepoch);
    method Action ma_commit_store(Tuple2#(Bit#(`desize), Bit#(TLog#(`dsbsize))) storecommit);
    method Action ma_commit_io(Bit#(`desize) currepoch);
  `ifdef supervisor
    interface Get#(DMem_core_response#(TMul#(`dwords,8),`desize)) get_ptw_resp;
    interface Put#(DTLB_core_response#(`paddr)) put_pa_from_tlb;
    interface Get#(DCache_core_request#(`vaddr, TMul#(`dwords, 8), `desize)) get_hold_req;
  `endif
  `ifdef perfmonitors
    method Bit#(13) mv_perf_counters;
  `endif

  `ifdef dcache_ecc
    method Maybe#(ECC_dcache_data#(`paddr, `dways, `dblocks)) mv_ded_data;
    method Maybe#(ECC_dcache_data#(`paddr, `dways, `dblocks)) mv_sed_data;
    method Maybe#(ECC_dcache_tag#(`paddr, `dways)) mv_ded_tag;
    method Maybe#(ECC_dcache_tag#(`paddr, `dways)) mv_sed_tag;
    method Action ma_ram_request(DRamAccess access);
    method Bit#(`respwidth) mv_ram_response;
  `endif
  endinterface : Ifc_dcache
  // both update rg_handling_miss but can never fire together
  (*conflict_free="rl_send_memory_request, rl_response_to_core"*)
  (*conflict_free="rl_response_to_core,rl_ram_check"*)
  (*conflict_free="rl_send_memory_request,rl_release_from_fillbuffer"*)
  (*conflict_free="rl_fill_from_memory, rl_release_from_fillbuffer"*)
  // the following rules access the mv_read_response from of data and tag modules which is a
  // conflict. However, these two rules will never fire together
  (*mutually_exclusive="rl_release_from_fillbuffer, rl_ram_check"*)
`ifdef dcache_ecc
  (*preempts="ma_ram_request,rl_release_from_fillbuffer"*)
  (*conflict_free="rl_perform_correction, ma_commit_store"*)
`endif
  // conflict is since both methods/rules call ma_increment_head method of sb. However, they can never call
  // ma_increment_head simultaneously.
  (*conflict_free="ma_commit_store, rl_commit_stores"*)
  (*conflict_free="ma_commit_store, rl_response_to_core"*)
  (*conflict_free="ma_commit_io, rl_io_response"*)
  // (*conflict_free="rl_response_to_core, rl_commit_stores"*)
  // both the following will update the replacement policy
  (*conflict_free="rl_release_from_fillbuffer, rl_response_to_core"*)
  // the following 2 conflict in responding to the ptw. however, only one of them can feed responses
  // to the PTW
  (*conflict_free="rl_response_to_core, rl_io_response"*)
  /*(*conflict_free="m_storebuffer_ma_allocate_entry, m_storebuffer_ma_increment_head"*)
  (*conflict_free="m_storebuffer_ma_commit_store, m_storebuffer_ma_increment_head"*)*/
`ifdef core_clkgate
(*synthesize,gate_all_clocks*)
`else
  (*synthesize*)
`endif
  module mkdcache#( parameter Bit#(32) id
    `ifdef pmp ,
        Vector#(`pmpentries, Bit#(8)) pmp_cfg, 
        Vector#(`pmpentries, Bit#(`paddr)) pmp_addr `endif
        `ifdef testmode ,Bool test_mode `endif )(Ifc_dcache);

    String dcache = "";
    let v_sets=valueOf(`dsets);
    let v_setbits=valueOf(`setbits);
    let v_wordbits=valueOf(`wordbits);
    let v_blockbits=valueOf(`blockbits);
    let v_linewidth=valueOf(`linewidth);
    let v_paddr=valueOf(`paddr);
    let v_ways=valueOf(`dways);
    let v_wordsize=valueOf(`dwords);
    let v_blocksize=valueOf(`dblocks);
    let v_respwidth=valueOf(`respwidth);
    let v_fbsize = valueOf(`dfbsize);
    let v_dbanks = valueOf(`dblocks);
    let v_tagbits = valueOf(`tagbits);
    let v_ecc_size = valueOf(`deccsize);

    let m_data <- mkdcache_data(id `ifdef testmode ,test_mode `endif );
    let m_tag <- mkdcache_tag(id `ifdef testmode ,test_mode `endif );
    let m_fillbuffer <- mkdcache_fb_v2(id);
    let m_storebuffer <- mkstorebuffer(id);
    let m_iobuffer <- mkiobuffer(id);
    // ----------------------- FIFOs to interact with interface of the design -------------------//
    /*doc:fifo: This fifo stores the request from the core.*/
    FIFOF#(DCache_core_request#(`vaddr, `respwidth, `desize)) ff_core_request <- mkSizedFIFOF(2);
    /*doc:fifo: This fifo stores the response that needs to be sent back to the core.*/
    FIFOF#(DMem_core_response#(`respwidth,`desize))ff_core_response <- mkBypassFIFOF();
    /*doc:reg: */
    Reg#(Maybe#(DMem_core_response#(`respwidth, `desize))) rg_core_io_response <- mkDReg(tagged Invalid);
  `ifdef supervisor
    /*doc:fifo: This fifo stores the response that needs to be sent back to the ptw.*/
    FIFOF#(DMem_core_response#(`respwidth,`desize))ff_ptw_response <- mkBypassFIFOF();
  `endif
    /*doc:fifo: this fifo stores the read request that needs to be sent to the next memory level.*/
    FIFOF#(DCache_mem_readreq#(`paddr)) ff_mem_rd_request <- mkSizedFIFOF(2);
    /*doc:fifo: This fifo stores the response from the next level memory.*/
    FIFOF#(DCache_mem_readresp#(`dbuswidth)) ff_mem_rd_resp  <- mkBypassFIFOF();
    /*doc:fifo: this fifo stores the eviction request to be written back*/
    FIFOF#(DCache_mem_writereq#(`paddr, `linewidth)) ff_mem_wr_request <- mkFIFOF1;
    /*doc:fifo: this fifo stores the write response from an eviction or a io write req*/
    FIFOF#(DCache_mem_writeresp) ff_mem_wr_resp  <- mkBypassFIFOF();
    /*doc:fifo: this fifo holds the request from core when there has been a tlbmiss */
    FIFOF#(DCache_core_request#(`vaddr, `respwidth, `desize)) ff_hold_request <- mkBypassFIFOF();
    /*doc:fifo: fifo to hold the IO requests going directly to the bus*/
    FIFOF#(DCache_io_req#(`paddr, `dbuswidth)) ff_mem_io_request <- mkFIFOF1();
    /*doc:fifo: fifo holding the IO responses coming directly from the bus*/
    FIFOF#(DCache_io_response#(`dbuswidth)) ff_mem_io_resp <- mkFIFOF1();
    

  `ifdef supervisor
    /*doc:fifo: this fifo receives the physical address from the TLB */
    FIFOF#(DTLB_core_response#(`paddr)) ff_from_tlb <- mkBypassFIFOF();
  `endif

    // ------------------------ FIFOs for internal state-maintenance ---------------------------//
    /*doc:fifo: This fifo holds meta information of the miss/io request that was made by the core*/
    FIFOF#(Pending_req#(`dfbsize, `dblocks)) ff_pending_req <- mkUGSizedFIFOF(2);
    
    // -------------------- Register declarations ----------------------------------------------//

    /*doc:reg: register when True indicates a fence is in progress and thus will prevent taking any
     new requests from the core*/
    Reg#(Bool) rg_fence_stall <- mkReg(False);

    /*doc:reg: When tru indicates that a miss is being catered to*/
    Reg#(Bool) rg_handling_miss <- mkReg(False);

    /*doc:reg: */
    Reg#(Bit#(1)) rg_wEpoch <- mkReg(0);
    
    /*doc:reg: this register indicates the read-phase of the release sequence*/
    Reg#(Bool) rg_release_readphase <- mkDReg(False);

    /*doc:reg: This register indicates that the bram inputs are being re-driven by those provided
    from the core in the most recent request. This happens because as the release from the
    fillbuffer happens it is possible that a dirty ways needs to be read out. This will change the
    output of the brams as compared to what the core requested. Thus the core request needs to be
    replayed on these again */
    Reg#(Bool) rg_performing_replay <- mkReg(False);

    /*doc:reg: this register holds the index of the most recent request performed by the core*/
    Reg#(Bit#(`setbits)) rg_recent_req <- mkReg(0);

    /*doc:reg: this register indicates that the line corresponding to the current request to the
    core is already persent however, the necessary is not present. This doesn't generate a miss
    and thus rg_miss_handling cannot be used here. Hence the need for this register*/
    Reg#(Bool) rg_polling_mode <- mkReg(False);

    /*doc:reg: this register selects the way for performing a fence operation */
    Reg#(Bit#(TLog#(`dways))) rg_fence_way <- mkReg(0);

    /*doc:reg: this register selects the set for performing a fence operation */
    Reg#(Bit#(TLog#(`dsets))) rg_fence_set <- mkReg(0);

    /*doc:reg: this register when true indicates that a fence operation has caused a writeback to
     the memory and the response has not been received yet.*/
    Reg#(Bool) rg_fence_pending <- mkReg(False);

    /*doc:reg: This register when true indicates that a there exists alteast one dirty line within
     the data cache */
    Reg#(Bool) rg_globaldirty <- mkReg(False);

    /*doc:reg: this register indicates that the io buffer has initiated an IO transaction and is
    * waiting for response*/
    Reg#(Bool) rg_io_busy <- mkReg(False);

    /*doc:reg: this register indicates that the IO op is part of an atomic op and the read phase is
    * over when this register is set.*/
    Reg#(Bool) rg_io_atomic_done <- mkReg(False);
    /*doc:reg: */
    Reg#(Bit#(`respwidth)) rg_atomic_rd_data <- mkReg(0);

  `ifdef dcache_ecc
    /*doc:reg: register to hold the access request performed by the external CCSU module*/
    Reg#(Maybe#(DRamAccess)) rg_access_req <- mkDReg(tagged Invalid);
    /*doc:reg: */
    Reg#(Bool) rg_perform_sec <- mkReg(False);
    /*doc:reg: */
    Reg#(Bool) rg_halt_ram_check <- mkReg(False);
    /*doc:reg: */
    Reg#(Bit#(TLog#(`dfbsize))) rg_sec_fbindex <- mkReg(0);
    /*doc:reg: */
    Reg#(Bit#(TMul#(`dblocks, `deccsize))) rg_sec_storeparity <- mkReg(0);
    Reg#(Bit#(TMul#(`dblocks, `deccsize))) rg_sec_checkparity <- mkReg(0);
    /*doc:reg: */
    Reg#(Bit#(`paddr)) rg_sec_address <- mkReg(0);

  `endif

  `ifdef atomic
    /*doc:reg: register holding the reservation address*/
    Reg#(Maybe#(Bit#(`vaddr))) rg_reservation_address <- mkReg(tagged Invalid);
  `endif

    // -------------------- Wire declarations ----------------------------------------------//
    /*doc:wire: boolean wire indicating if the cache is enabled. This is controlled through a csr*/
    Wire#(Bool) wr_cache_enable<-mkWire();
    /*doc:wire: this wire indicates if there was a fault in the address or during translation*/
    Wire#(Bool) wr_fault <- mkDWire(False);
    /*doc:wire: this wire indicates if there was a hit or miss on SRAMs.*/
    Wire#(RespState) wr_ram_state <- mkDWire(None);
    /*doc:wire: this wire holds the response from the RAM in case of a hit in the RAMs*/
    Wire#(DMem_core_response#(`respwidth,`desize)) wr_ram_response <- mkDWire(?);
    /*doc:wire: in case of a hit in the ram, this wire holds the information of which way was a hit.
    This is used for replacement purposes only.*/
    Wire#(Bit#(TLog#(`dways))) wr_ram_hitway <-mkDWire(0);
    /*doc:wire: in case of a store-hit in the RAM, the hit line needs to be transfered to the FB.
    This wire holds that hit line*/
    Wire#(Bit#(`linewidth)) wr_ram_hitline <- mkDWire(?);
    /*doc:wire in case of a hit in the rams, the wire holds the holds the value of the set which
    caused a hit. This is necessary since an eviction from the same set should not affect the
    replacement policy if a hit to the same set has occurred in the same cycle */
    Wire#(Maybe#(Bit#(`setbits))) wr_ram_hitset <- mkDWire(tagged Invalid);

    /*doc:wire: this wire indicates if there was a hit or miss on Fllbuffer.*/
    Wire#(RespState) wr_fb_state <- mkDWire(None);

    /*doc:wire: this wire holds the response data structure in case of a hit from fill-buffers*/
    Wire#(DMem_core_response#(`respwidth,`desize)) wr_fb_response <- mkDWire(?);

  `ifdef perfmonitors
    /*doc:wire: wire to pulse on every read access*/
    Wire#(Bit#(1)) wr_total_read_access <- mkDWire(0);
    /*doc:wire: wire to pulse on every write access*/
    Wire#(Bit#(1)) wr_total_write_access <- mkDWire(0);
    /*doc:wire: wire to pulse on every atomic access*/
    Wire#(Bit#(1)) wr_total_atomic_access <- mkDWire(0);
    /*doc:wire: wire to pulse on every io read access*/
    Wire#(Bit#(1)) wr_total_io_reads <- mkDWire(0);
    /*doc:wire: wire to pulse on every io write access*/
    Wire#(Bit#(1)) wr_total_io_writes <- mkDWire(0);
    /*doc:wire: wire to pulse on every read miss within the cache*/
    Wire#(Bit#(1)) wr_total_read_miss <- mkDWire(0);
    /*doc:wire: wire to pulse on every write miss within the cache*/
    Wire#(Bit#(1)) wr_total_write_miss <- mkDWire(0);
    /*doc:wire: wire to pulse on every atomic miss within the cache*/
    Wire#(Bit#(1)) wr_total_atomic_miss <- mkDWire(0);
    /*doc:wire: wire to pulse on every eviction from the cache*/
    Wire#(Bit#(1)) wr_total_evictions <- mkDWire(0);
    /*doc:wire: wire to pulse on every release from fill-buffer to RAMS*/
    Wire#(Bit#(1)) wr_total_fb_releases <- mkDWire(0);
    /*doc:wire: wire to pulse on every hit in fill-buffer for atomic ops*/
    Wire#(Bit#(1)) wr_total_atomic_fb_hits <- mkDWire(0);
    /*doc:wire: wire to pulse on  hit in fill-buffer for read ops*/
    Wire#(Bit#(1)) wr_total_read_fb_hits <- mkDWire(0);
    /*doc:wire: wire to pulse on  hit in fill-buffer for write ops*/
    Wire#(Bit#(1)) wr_total_write_fb_hits <- mkDWire(0);
  `endif
    Wire#(Bool) wr_takingrequest <- mkDWire(False);
    /*doc:wire: in case of a hit in fb this wire will hold the index of the fb which was a hit. This
    value is used to indicate the storebuffer which fb entry it needs to update when committing
    the store*/
    Wire#(Bit#(TLog#(`dfbsize))) wr_fb_hitindex <- mkDWire(?);
    /*doc:wire: wire holds the current privilege mode of the core*/
    Wire#(Bit#(2)) wr_priv <- mkWire();

  `ifdef dcache_ecc
    /*doc:wire: */
    Wire#(Bool) wr_ecc_fault <- mkDWire(False);
    /*doc:wire: */
    Wire#(Maybe#(ECC_dcache_tag#(`paddr,`dways))) wr_sed_tag_log <- mkDWire(tagged Invalid);
    /*doc:wire: */
    Wire#(Maybe#(ECC_dcache_tag#(`paddr,`dways))) wr_ded_tag_log <- mkDWire(tagged Invalid);
    /*doc:wire: */
    Wire#(Maybe#(ECC_dcache_data#(`paddr,`dways, `dblocks))) wr_sed_data_log <- mkDWire(tagged Invalid);
    /*doc:wire: */
    Wire#(Maybe#(ECC_dcache_data#(`paddr,`dways, `dblocks))) wr_ded_data_log <- mkDWire(tagged Invalid);
  `endif
    // ----------------------- Storage elements -------------------------------------------//
    /*doc:reg: This is an array of the valid bits. Each entry corresponds to a set and contains
    'way' number of bits in each entry*/
    Vector#(`dsets, Reg#(Bit#(`dways))) v_reg_valid <- replicateM(mkReg(0));

    /*doc:reg: This is an array of the dirty bits. Each entry corresponds to a set and contains
    'way' number of bits in each entry*/
    Vector#(`dsets, Reg#(Bit#(`dways))) v_reg_dirty <- replicateM(mkReg(0));

    Ifc_replace#(`dsets,`dways) replacement <- mkreplace(`drepl);
    // --------------------- Store buffer related structures ----------------------------------//

    /*doc:wire: when true indicates that a store-buffer entry is being allocated. This is used to
     ensure that a release of the fill-buffer does not happen.*/
    Wire#(Bool) wr_allocating_storebuffer <- mkDWire(False);

  `ifdef dcache_ecc
    Vector#(`dblocks, Wrapper3#(Bit#(`deccsize), Bit#(`deccsize), 
      Bit#(`respwidth), Bit#(`respwidth))) fn_ecc_correct_uw <-
      replicateM(mkUniqueWrapper3(fn_ecc_correct));
  `endif

    // --------------------------- global variables ------------------------------------- //
    Bool sb_empty = m_storebuffer.mv_sb_empty;
    Bool sb_full = m_storebuffer.mv_sb_full;
    Bool sb_busy = m_storebuffer.mv_sb_busy;
    Bool io_empty = m_iobuffer.mv_io_empty;
    Bool io_full  = m_iobuffer.mv_io_full;
    Bool fb_full = m_fillbuffer.mv_fbfull;
    Bool fb_empty = m_fillbuffer.mv_fbempty;
    Bool fb_headvalid = m_fillbuffer.mv_fbhead_valid;
    // --------- release information 
    let lv_release_line = m_fillbuffer.mv_release_info.dataline;
    let lv_release_addr = m_fillbuffer.mv_release_info.address;
    let lv_release_err  = m_fillbuffer.mv_release_info.err;
    let lv_release_dirty =m_fillbuffer.mv_release_info.dirty; 

    Bit#(`setbits) fillindex = lv_release_addr[v_setbits + v_blockbits + v_wordbits - 1:
                                                                          v_blockbits + v_wordbits];
    /*doc:var: This variable indicates if there is an oppurtunity to perform a release from the
    fill-buffer to the RAMS. This takes advantage of the fact that the cache is idle is not being
    used by the core. The conditions under which an oppurtunity occurs is if all the following
    conditions are met:
      1. there is not core-request pending
      2. The core is not generating any request in the current cycle
      3. Store-buffer is not being allocated in the current cycle
      4. The set being released to is not the most recent set accessed by the core.
    */
    Bool fill_oppurtunity = (!ff_core_request.notEmpty && !wr_takingrequest)  &&
         /*countOnes(fb_valid)>0 &&*/ (fillindex != rg_recent_req) && !wr_allocating_storebuffer;
    
    // --------------------------- Rule operations ------------------------------------- //

    rule rl_fence_operation(ff_core_request.first.fence && rg_fence_stall && fb_empty && 
                            sb_empty && io_empty && !rg_fence_pending && !rg_performing_replay
                          `ifdef dcache_ecc && !rg_perform_sec && !rg_halt_ram_check `endif ) ;
      `logLevel( dcache, 1, $format("[%2d]DCACHE : Fence operation in progress",id))

      let lv_curr_way = rg_fence_way;
      let lv_curr_set = rg_fence_set;

      let lv_next_way = rg_fence_way;
      let lv_next_set = {1'b0,rg_fence_set};

      // done to avoid additional provisos for this combination
      Bit#(TSub#(`paddr, TAdd#(`tagbits, `setbits))) zeros = 'd0;
      Bit#(`dways) _way = 0;
      _way[rg_fence_way] = 1;
      let lv_tag_resp = m_tag.mv_tag_select(rg_fence_way);
      let lv_data_resp <- m_data.mv_line_select(_way);
      Bit#(`tagbits) tag = truncateLSB(lv_tag_resp.tag);
      Bit#(`linewidth) dataline = lv_data_resp.line;
      Bit#(`paddr) final_address={tag, rg_fence_set, zeros};
    `ifdef dcache_ecc
      Bit#(TMul#(`dblocks,TAdd#(2,TLog#(`respwidth)))) stored_parity = lv_data_resp.stored_parity;
      Bit#(TMul#(`dblocks,TAdd#(2,TLog#(`respwidth)))) check_parity = lv_data_resp.check_parity;
      for (Integer i = 0; i< v_blocksize; i = i + 1) begin
        Bit#(ecc_size) _stparity = stored_parity[i*v_ecc_size+v_ecc_size-1:i*v_ecc_size];
        Bit#(ecc_size) _chparity = check_parity[i*v_ecc_size+v_ecc_size-1:i*v_ecc_size];
        Bit#(`respwidth) _data = dataline[i*v_respwidth+v_respwidth-1:i*v_respwidth];
        _data <- fn_ecc_correct_uw[i].func(_chparity, _stparity, _data);
        dataline[i*v_respwidth+v_respwidth-1:i*v_respwidth] = _data;
      end
      if(|lv_tag_resp.ded == 1)
        wr_ded_tag_log <= tagged Valid ECC_dcache_tag{address: final_address, 
                                                     way: lv_tag_resp.ded & v_reg_valid[rg_fence_set]};

      if (|lv_data_resp.line_ded == 1)
        wr_ded_data_log <= tagged Valid ECC_dcache_data{address: final_address, 
                                                       banks: lv_data_resp.line_ded,
                                                       way : _way};
    `endif
      Bit#(1) lv_dirty = v_reg_dirty[rg_fence_set][rg_fence_way];
      Bit#(1) lv_valid = v_reg_valid[rg_fence_set][rg_fence_way];
      `logLevel( dcache, 2, $format("[%2d]DCACHE: Fence: CurrWay:%2d CurrSet:%2d Valid:%b \
Dirty:%b Addr:%h",id, lv_curr_way,lv_curr_set,lv_valid, lv_dirty, final_address))
      Bool writeback_condition = lv_dirty == 1 && lv_valid == 1;
      if( writeback_condition) begin
        let lv_req = DCache_mem_writereq{address   : final_address,
                                         burst_len  : fromInteger(((`dblocks * `dwords * 8) / `dbuswidth)  - 1 ),
                                         burst_size : fromInteger(valueOf(TLog#(TDiv#(`dbuswidth,8)))),
                                         data       : dataline
                                          };
        ff_mem_wr_request.enq(lv_req);
        `logLevel( dcache, 2, $format("[%2d]DCACHE: Fence: Evicting to Memory:",id,fshow(lv_req)))
      end
      if(lv_curr_way == fromInteger(v_ways-1))
        lv_next_set = zeroExtend(lv_curr_set) + 1;

      if(v_ways > 1)
        lv_next_way = lv_curr_way + 1;

      m_tag.ma_request(False, truncate(lv_next_set), lv_release_addr , ?);
      m_data.ma_request(False, truncate(lv_next_set), lv_release_line, ?, '1);

      if((lv_curr_way == fromInteger(v_ways - 1) && lv_next_set== fromInteger(v_sets))
              || !rg_globaldirty) begin
        for (Integer i = 0; i< fromInteger(v_sets); i = i + 1) begin
          v_reg_valid[i] <= 0 ;
          v_reg_dirty[i] <= 0 ;
        end
        rg_globaldirty <= False;
        rg_fence_stall <= False;
        ff_core_request.deq;
        replacement.reset_repl;
        rg_fence_way <= 0;
        rg_fence_set <= 0;
        `logLevel( dcache, 0, $format("DCACHE[%2d]: Ending Fence op",id))
        ff_core_response.enq(DMem_core_response{word:?, trap: False, is_io: False,
                              cause: ?, epochs: ff_core_request.first.epochs,
                              sb_id: ?,
                              entry_alloc: False});
      end
      else begin
        rg_fence_way <= lv_next_way;
        rg_fence_set <= truncate(lv_next_set);
      end
    endrule
    /*doc:rule: */
    rule rl_deq_write_resp(rg_fence_pending && ff_core_request.first.fence);
      rg_fence_pending <= False;
      let x = ff_mem_wr_resp.first;
    endrule
    /*doc:rule: whether the write response is for a fence or is for eviction it has to be evicted.
     Hence this has been decoupled from the previous rule - which is meant only for fence*/
    rule rl_deq_write_response;
      ff_mem_wr_resp.deq;
    endrule

  `ifdef dcache_ecc
    /*doc:rule: */
    rule rl_perform_correction(rg_perform_sec);
      rg_perform_sec <= False; 
      m_fillbuffer.mav_perform_sec(rg_sec_fbindex, rg_sec_storeparity, rg_sec_checkparity);
      rg_halt_ram_check <= True;
      `logLevel( dcache, 0, $format("[%2d]DCACHE: Performing SEC for fbindex:%d",id, rg_sec_fbindex))
    endrule
  `endif
    /*doc:rule: This rule checks the tag rams for a hit*/
    rule rl_ram_check(!ff_core_request.first.fence && !rg_handling_miss && !rg_performing_replay
                      && !rg_polling_mode && !fb_full && !io_full && !rg_release_readphase
                  `ifdef atomic && !sb_busy `endif 
                  `ifdef dcache_ecc && !rg_perform_sec && !rg_halt_ram_check `endif );
      let req = ff_core_request.first;
      // select the physical address and check for any faults
    `ifdef supervisor
      let pa_response = ff_from_tlb.first;
      Bit#(`paddr) phyaddr = pa_response.address;
      Bool lv_access_fault = pa_response.trap || pa_response.tlbmiss;
      Bit#(`causesize) lv_cause = lv_access_fault? pa_response.cause:
                                  req.access == 0?`Load_access_fault:`Store_access_fault;
      `logLevel( dcache, 1, $format("[%2d]DCACHE: TLB Resp:",id,fshow(pa_response)))
    `else
      Bit#(TSub#(`vaddr,`paddr)) upper_bits=truncateLSB(req.address);
      Bit#(`paddr) phyaddr = truncate(req.address);
      Bool lv_access_fault = unpack(|upper_bits);
      Bit#(`causesize) lv_cause = req.access == 0?`Load_access_fault:`Store_access_fault;
    `endif
    `ifdef pmp
      Bit#(2) pmp_access = req.access == 0 ? 0 : 1;
      let pmpreq = PMPReq{ address: phyaddr, access_type:pmp_access};
      let {pmp_err, pmp_cause} = fn_pmp_lookup(pmpreq, unpack(wr_priv),
                                              pmp_cfg, pmp_addr);
      if (!lv_access_fault && pmp_err)begin
        lv_access_fault = True;
        lv_cause = pmp_cause;
      end
    `endif
      `logLevel( dcache, 2, $format("[%2d]DCACHE: RAM Processing:",id,fshow(req)))
      Bit#(`blockbits) lv_blocknum = phyaddr[v_blockbits+v_wordbits-1:v_wordbits];
      Bit#(`wordbits) word_offset = truncate(phyaddr);
      Bit#(`setbits) set_index= phyaddr[v_setbits+v_blockbits+v_wordbits-1:v_blockbits+v_wordbits];

      let lv_tag_resp = m_tag.mv_tagmatch_resp(phyaddr);
      Bit#(`dways) lv_hitmask = lv_tag_resp.waymask & v_reg_valid[set_index];
      let lv_data_resp <- m_data.mv_word_select(lv_blocknum,lv_hitmask);
      let response_word = lv_data_resp.word >> {word_offset,3'b0};
      if(|lv_hitmask==0)
        `logLevel( dcache, 0, $format("[%2d]DCACHE: TagRAM: Miss",id))
      else begin
        `logLevel( dcache, 0, $format("[%2d]DCACHE: TagRAM Hit in line:%b",id,lv_hitmask))
        `logLevel( dcache, 0, $format("[%2d]DCACHE: DataRAM Selected Word:%h",id,lv_data_resp.word))
      end

    `ifdef dcache_ecc

      rg_sec_checkparity <= lv_data_resp.check_parity;
      rg_sec_storeparity <= lv_data_resp.stored_parity;

      if(|(lv_tag_resp.ded & v_reg_valid[set_index]) == 1)
        lv_access_fault = True;
      else if(|lv_hitmask == 1 && |lv_data_resp.line_ded == 1)
        lv_access_fault = True;

      if(|lv_tag_resp.ded == 1)
        wr_ded_tag_log <= tagged Valid ECC_dcache_tag{address: phyaddr, 
                                                     way: lv_tag_resp.ded & v_reg_valid[set_index]};

      if (|lv_data_resp.line_ded == 1 && |lv_hitmask == 1)
        wr_ded_data_log <= tagged Valid ECC_dcache_data{address: phyaddr, 
                                                       banks: lv_data_resp.line_ded,
                                                       way : lv_hitmask};

      if (|(lv_tag_resp.sed & lv_hitmask) == 1)
        wr_sed_tag_log <= tagged Valid ECC_dcache_tag{address: phyaddr,
                                                    way: lv_hitmask};

      if (|lv_data_resp.line_sed == 1 && |lv_hitmask == 1)
        wr_sed_data_log <= tagged Valid ECC_dcache_data{address: phyaddr, 
                                                       banks: lv_data_resp.line_sed,
                                                       way : lv_hitmask};
    `endif
      let lv_response = DMem_core_response{word:response_word, trap: lv_access_fault,
                                          is_io: False, cause: lv_cause, epochs: req.epochs,
                                          entry_alloc: req.access !=0 };

      wr_ram_response <= lv_response;
      wr_ram_hitway <= truncate(pack(countZerosLSB(lv_hitmask)));
      wr_ram_hitline <= lv_data_resp.line;

      if(lv_access_fault) begin
        wr_fault <= True;
      end
      else if(|(lv_hitmask) == 1 && wr_cache_enable) begin// trap or hit in RAMs
        wr_ram_state <= Hit;
      end
      else begin // in case of miss from cache
        wr_ram_state <= Miss;
      end

    `ifdef ASSERT
      dynamicAssert(countOnes(lv_hitmask) <= 1,"DCACHE: More than one way is a hit in the cache");
    `endif
    endrule
    rule rl_fillbuffer_check(!ff_core_request.first.fence && !fb_full && !io_full
                              `ifdef atomic && !sb_busy `endif 
                              `ifdef dcache_ecc && !rg_perform_sec `endif );
      let req = ff_core_request.first;
      `logLevel( dcache, 2, $format("[%2d]DCACHE: FB Processing :",id,fshow(req)))
    `ifdef supervisor
      Bit#(`paddr) phyaddr = ff_from_tlb.first.address;
    `else
      Bit#(`paddr) phyaddr = truncate(req.address);
    `endif
      Bit#(`wordbits) word_offset = truncate(phyaddr);
      Bit#(`causesize) lv_cause = req.access == 0? `Load_access_fault: `Store_access_fault;

      let lv_polling_resp <- m_fillbuffer.mav_polling_response(phyaddr, ff_pending_req.notEmpty, 
                ff_pending_req.first.fbindex);
      let lv_io_req = isIO(phyaddr, wr_cache_enable);

      let lv_response_word = lv_polling_resp.word >> {word_offset, 3'b0};
      let lv_hitmask = lv_polling_resp.waymask;
      let lv_linehit = lv_polling_resp.line_hit;
      let lv_wordhit = lv_polling_resp.word_hit;
      let lv_response_err = lv_polling_resp.err;

      wr_allocating_storebuffer <= req.access != 0 && !lv_io_req;
      wr_fb_hitindex <= truncate(pack(countZerosLSB(lv_hitmask)));
      let lv_response = DMem_core_response{word:lv_response_word, trap: unpack(lv_response_err),
                                          is_io: lv_io_req, cause: lv_cause, epochs: req.epochs,
                                          entry_alloc: lv_io_req || req.access !=0 };
      if(lv_io_req) begin
        wr_fb_state <= Hit;
        wr_fb_response <= lv_response;
        `logLevel( dcache, 1, $format("[%2d]DCACHE: FB Detected NC OP ",id))
      end
      else if(lv_linehit)begin
        if(lv_wordhit)begin
          wr_fb_state <= Hit;
          wr_fb_response <= lv_response;
          `logLevel( dcache, 1, $format("[%2d]DCACHE: FB Required word found in line:%b",id,
                                                                                      lv_hitmask))
          rg_polling_mode <= False;
        end
        else begin
          wr_fb_state <= None;
          rg_polling_mode <= True;
          `logLevel( dcache, 1, $format("[%2d]DCACHE: FB Required word not available yet in line:%b", id, lv_hitmask))
        end
      end
      else begin
        wr_fb_state <= Miss;
        rg_polling_mode <= False;
        `logLevel( dcache, 1, $format("[%2d]DCACHE: FB: Miss",id))
      end
    endrule

    /*doc:rule: this rule fires when the requested word is either present in the SRAMs or the
    fill-buffer or if there was an error in the request. Since we are re-using the
    ff_mem_wr_resp fifo to send out cacheable and MMIO ops, it is necessary that we make sure
    that this fifo is not Full before responding back to the core. If it is not empty then the core
    could initiate a commit-store which could get dropped since the method performing the cannot
    fire since the fifo is full and thus the store being dropped.*/
    rule rl_response_to_core(!ff_core_request.first.fence && `ifdef dcache_ecc !rg_perform_sec && `endif 
                      ( wr_fault || wr_ram_state == Hit || wr_fb_state == Hit));

    `ifdef dcache_ecc
      Bool lv_tag_sed = isValid(wr_sed_tag_log);
      Bool lv_data_sed = isValid(wr_sed_data_log);
      rg_halt_ram_check <= False;
    `endif
      Bool _faulty =  `ifdef dcache_ecc 
                          ((lv_data_sed || lv_tag_sed) && wr_ram_state == Hit) 
                        `else 
                          False 
                        `endif ;

      let req = ff_core_request.first;
    `ifdef supervisor
      let pa_response = ff_from_tlb.first;
      Bit#(`paddr) phyaddr = pa_response.address;
    `else
      Bit#(`paddr) phyaddr = truncate(req.address);
    `endif
      Bit#(`setbits) set_index= phyaddr[v_setbits+v_blockbits+v_wordbits-1:v_blockbits+v_wordbits];
      Bit#(`wordbits) word_offset = truncate(phyaddr);
      DMem_core_response#(`respwidth,`desize) lv_response;

      let {storemask, storedata} <- m_storebuffer.mav_check_sb_hit(phyaddr);
      Bit#(2) onehot_hit = {pack(wr_ram_state==Hit || wr_fault), 
                            pack(wr_fb_state==Hit && !wr_fault)};
    `ifdef ASSERT
      if(!wr_fault)
        dynamicAssert(countOnes(onehot_hit) == 1, "More than one data structure shows a hit");
    `endif
      Vector#(2, DMem_core_response#(`respwidth,`desize)) lv_responses;
      lv_responses[0] = wr_fb_response;
      lv_responses[1] = wr_ram_response;

      lv_response = select(lv_responses,unpack(onehot_hit));
      lv_response.sb_id = m_storebuffer.mv_sb_curr_tail;

      if(wr_ram_state == Hit && !wr_fault) begin
        `logLevel( dcache, 0, $format("[%2d]DCACHE: Response Hit from SRAM",id))
        if(`drepl == 2) begin
          replacement.update_set(set_index, wr_ram_hitway);//wr_replace_line);
          wr_ram_hitset <= tagged Valid set_index;
        end
      end
      if(wr_fb_state == Hit && !wr_fault) begin
        `logLevel( dcache, 0, $format("[%2d]DCACHE: Response Hit from Fillbuffer",id))
      `ifdef perfmonitors
        if(rg_handling_miss) begin
          if(req.access == 0)
            wr_total_read_fb_hits <= 1;
          if(req.access == 1)
            wr_total_write_fb_hits<= 1;
          `ifdef atomic
            if(req.access == 2)
              wr_total_atomic_fb_hits <= 1;
          `endif
        end
      `endif
      end
   
      Bool skip_allocation = False;
    `ifdef atomic
      if (req.access == 2 && req.atomic_op[3:0]=='b0101) begin// LR op
        rg_reservation_address <= tagged Valid (req.address & `reservation_mask);
        req.access = 0;
        lv_response.entry_alloc = lv_response.is_io;
        `logLevel( dcache, 0, $format("[%2d]DCACHE: LR reservation for : %h",id,req.address))
      end
      else if (req.access == 2 && req.atomic_op[3:0] == 'b0111) begin // SC op
        rg_reservation_address <= tagged Invalid;
        if (rg_reservation_address matches tagged Valid .resaddr &&& 
                                                resaddr  == (req.address & `reservation_mask))begin
          req.access = 1; // change this op to store
          lv_response.word = 0;
          `logLevel( dcache, 0, $format("[%2d]DCACHE: SC succeeds for : %h ",id,req.address))
        end
        else begin
          req.access = 0; // change this op to load and exit
          lv_response.word = 1;
          lv_response.entry_alloc = False;
          skip_allocation = True;
          `logLevel( dcache, 0, $format("[%2d]DCACHE: SC fails for : %h. ResAddr:%h",id,req.address,
            fromMaybe(?,rg_reservation_address)))
        end
      end
    `endif


      lv_response.word = (storemask & storedata) | (~storemask & lv_response.word);
      // capture the sign bit of the response to the core
      Bit#(1) lv_sign =case(req.size[1:0])
          'b00: lv_response.word[7];
          'b01: lv_response.word[15];
          default: lv_response.word[31];
        endcase;
      // manipulate the sign based on the request of the core
      lv_sign = lv_sign & ~req.size[2];

      // generate a mask based on the request of the core.
      Bit#(respwidth) mask = case(req.size[1:0])
        'b00: 'hFF;
        'b01: 'hFFFF;
        'b10: 'hFFFFFFFF;
        default: '1;
      endcase;

      // signmask basically has all bits which are zeros in the mask duplicated with the required
      // sign bit. Theese need to be set in the final response to the core and will thus be ORed
      Bit#(respwidth) signmask = ~mask & duplicate(lv_sign);
      lv_response.word = (lv_response.word & mask) | signmask;
      lv_response.word = (lv_response.trap)?truncate(req.address):lv_response.word;

      if(!_faulty)begin
        ff_core_request.deq;
      `ifdef supervisor
        ff_from_tlb.deq;
        if(pa_response.tlbmiss)
          ff_hold_request.enq(ff_core_request.first());
        else if(req.ptwalk_req && !pa_response.tlbmiss) begin
          if (!lv_response.is_io)
            ff_ptw_response.enq(lv_response);
        end
        else
      `endif
        ff_core_response.enq(lv_response);
        rg_handling_miss <= False;
      end

      // -- allocate io-buffer entry
      if (lv_response.is_io && !skip_allocation) begin
        `logLevel( dcache, 0, $format("[%2d]DCACHE: Allocating IO Buffer",id))
        m_iobuffer.ma_allocate_io(IoEntry{address: phyaddr, data: req.data, epoch:req.epochs,
                                              size: req.size, access: truncate(req.access)
                                          `ifdef atomic ,atomic_op: req.atomic_op `endif
                                          `ifdef supervisor 
                                              ,is_ptw_req: req.ptwalk_req 
                                              ,vaddr: req.address
                                          `endif 
                                    });
      end
      // -- allocate store-buffer for stores/atomic ops
      Bit#(TLog#(`dfbsize)) _fbindex = ?;
      if( (req.access != 0 || _faulty ) && !skip_allocation
                      && wr_ram_state == Hit && !wr_fault) begin
        _fbindex <- m_fillbuffer.mav_allocate_line(True, wr_ram_hitline, phyaddr,
                                                       v_reg_dirty[set_index][wr_ram_hitway]);

        // invalidate the entries in the RAM since they not reside inside the FB
        v_reg_valid[set_index][wr_ram_hitway] <= 1'b0;
        v_reg_dirty[set_index][wr_ram_hitway] <= 1'b0;
      `ifdef dcache_ecc
        if (_faulty) begin
          rg_perform_sec <= True;
          rg_sec_fbindex <= _fbindex;
          rg_sec_address <= phyaddr;
          `logLevel( dcache, 0, $format("[%2d]DCACHE: Detected Single Error in Data Line",id))
        end
      `endif
      end
    `ifdef supervisor
      if(!pa_response.tlbmiss)
    `endif
    if(!_faulty `ifdef supervisor && !(req.ptwalk_req && !pa_response.tlbmiss && lv_response.is_io) `endif )
        `logLevel( dcache, 0, $format("[%2d]DCACHE: Responding :",id, fshow(lv_response)))

      if( !lv_response.is_io && req.access!=0 && !_faulty && !lv_response.trap  && !skip_allocation
                                          `ifdef supervisor && !pa_response.tlbmiss `endif )begin
        Bit#(TLog#(`dfbsize)) fbindex = (wr_fb_state == Hit && !wr_fault)? wr_fb_hitindex:_fbindex;
        m_storebuffer.ma_allocate_entry(phyaddr,req.data, req.epochs, fbindex, truncate(req.size)
                                `ifdef atomic ,req.access == 2,lv_response.word, req.atomic_op `endif );
        `logLevel( dcache, 0, $format("[%2d]DCACHE: Allocating Store Buffer",id))
      end
    endrule
    
    /*doc:rule: This rule fires when the requested word is a miss in both the SRAMs and the
    Fill-buffer. This rule thereby forwards the requests to the network. IOs by default should
    be a miss in both the SRAMs and the FB and thus need to be checked only here */
    rule rl_send_memory_request(wr_ram_state == Miss && wr_fb_state == Miss && !fb_full &&
          !wr_fault && !rg_handling_miss && ! ff_core_request.first.fence && ff_pending_req.notFull 
          `ifdef dcache_ecc && !rg_perform_sec `endif );
      let req = ff_core_request.first;
    `ifdef supervisor
      let pa_response = ff_from_tlb.first;
      Bit#(`paddr) phyaddr = pa_response.address;
    `else
      Bit#(`paddr) phyaddr = truncate(req.address);
    `endif
      let shift_amount = valueOf(TLog#(TDiv#(`dbuswidth,8)));
      Bit#(`paddr) blockmask = '1 << shift_amount;
      Bit#(`blockbits) lv_blocknum = phyaddr[v_blockbits+v_wordbits-1:v_wordbits] & blockmask[v_blockbits+v_wordbits-1:v_wordbits];
      // allocate a pending req which points to the new fb entry that is allotted.
      // align the address to be line-address aligned
      phyaddr = phyaddr & blockmask;
      ff_mem_rd_request.enq(DCache_mem_readreq{ address   : phyaddr,
          burst_len:fromInteger(((`dblocks * `dwords * 8) / `dbuswidth)  - 1 ),
          burst_size : fromInteger(valueOf(TLog#(TDiv#(`dbuswidth,8))))
         });
      rg_handling_miss <= True;
      Bit#(TLog#(`dfbsize)) lv_alotted_fb = ?;

      // -- allocate a new entry in the fillbuffer
      lv_alotted_fb <- m_fillbuffer.mav_allocate_line(False, ?, phyaddr, ?);
      `logLevel( dcache, 0, $format("[%2d]DCACHE: Allocating Fbindex:%d",id, lv_alotted_fb))
      let pend_req = Pending_req{init_bank: lv_blocknum,
                                fbindex: lv_alotted_fb};
      ff_pending_req.enq(pend_req);
    `ifdef perfmonitors
      wr_total_io_reads <= pack(req.access == 0);
      wr_total_io_writes <= pack(req.access == 1);
      wr_total_read_miss <= pack(req.access == 0);
      wr_total_write_miss <= pack(req.access == 1);
      `ifdef atomic
      wr_total_atomic_miss <= pack(req.access == 2);
      `endif
    `endif
        `logLevel( dcache, 0, $format("[%2d]DCACHE: Sending Line Request for Addr:%h",id, phyaddr))
    endrule
    /*doc:rule: this rule will fill up the FB with the response from the memory, Once the last word
    has been received the entire line and tag are written in to the BRAM and the fill buffer is
    released in the next cycle*/
    rule rl_fill_from_memory(ff_pending_req.notEmpty);
      let pending_req = ff_pending_req.first;
      let response = ff_mem_rd_resp.first;
      ff_mem_rd_resp.deq;
      m_fillbuffer.ma_fill_from_memory(response, pending_req.fbindex, pending_req.init_bank);
      if(response.last)
        ff_pending_req.deq;
    endrule
    rule rl_perform_replay(rg_performing_replay);
      m_tag.ma_request(False, rg_recent_req, lv_release_addr, ?);
      m_data.ma_request(False, rg_recent_req, lv_release_line, ?, '1);
      rg_performing_replay <= False;
      `logLevel( dcache, 0, $format("[%2d]DCACHE: Replaying Req. set:%d",id,rg_recent_req))
    endrule
    rule rl_release_from_fillbuffer((fb_full || rg_fence_stall || fill_oppurtunity) && 
                                    sb_empty && !fb_empty && !wr_allocating_storebuffer  
                                    && fb_headvalid && !rg_performing_replay);
      let addr = lv_release_addr;
      Bit#(`setbits) set_index = addr[v_setbits + v_blockbits + v_wordbits - 1 :
                                                                         v_blockbits + v_wordbits];

      let waynum <- replacement.line_replace(set_index, v_reg_valid[set_index],
                                                       v_reg_dirty[set_index]);
      `logLevel( dcache, 2, $format("[%2d]DCACHE: Release: set:%d way:%d valid:%b dirty:%b",id,
                                    set_index, waynum,v_reg_valid[set_index][waynum] ,
                                    v_reg_dirty[set_index][waynum] ))
      if(lv_release_err == 0)begin
        // enter here if the fillbuffer entry is valid and has no errors
        if((v_reg_valid[set_index][waynum] & v_reg_dirty[set_index][waynum])==1 &&
                                                                        !rg_release_readphase)begin
          // enter here if the line to be replaced is valid and dirty. We thus need to first read it
          // out and then send to the next level
          m_tag.ma_request(False, set_index, lv_release_addr, ?);
          m_data.ma_request(False, set_index, lv_release_line, ?, '1);
          rg_release_readphase <= True;
          `logLevel( dcache, 0, $format("[%2d]DCACHE: Release: Reading dirty set:%d way:%d",id,
                                      set_index,waynum))
        end
        else if((v_reg_valid[set_index][waynum] & v_reg_dirty[set_index][waynum]) !=1 ||
                                                                        rg_release_readphase)begin
          // enter here if either the entry being replaced is not dirty or if its dirty then it has
          // been read out of the ram in the previous cycle and is ready for eviction
          Bit#(TSub#(`paddr,TAdd#(`tagbits,`setbits))) zeros = 0;
          Bit#(`dways) _way = 0;
          _way[waynum] = 1;
          let lv_tag_resp = m_tag.mv_tag_select(waynum);
          let lv_data_resp <- m_data.mv_line_select(_way);
          Bit#(`tagbits) tag = truncateLSB(lv_tag_resp.tag);
          Bit#(`linewidth) dataline = lv_data_resp.line;
          Bit#(`paddr) lv_evict_address = {tag,set_index,zeros};
        `ifdef dcache_ecc
          Bit#(TMul#(`dblocks,TAdd#(2,TLog#(`respwidth)))) stored_parity = lv_data_resp.stored_parity;
          Bit#(TMul#(`dblocks,TAdd#(2,TLog#(`respwidth)))) check_parity = lv_data_resp.check_parity;
          for (Integer i = 0; i< v_blocksize; i = i + 1) begin
            Bit#(ecc_size) _stparity = stored_parity[i*v_ecc_size+v_ecc_size-1:i*v_ecc_size];
            Bit#(ecc_size) _chparity = check_parity[i*v_ecc_size+v_ecc_size-1:i*v_ecc_size];
            Bit#(`respwidth) _data = dataline[i*v_respwidth+v_respwidth-1:i*v_respwidth];
            _data <- fn_ecc_correct_uw[i].func(_chparity, _stparity, _data);
            dataline[i*v_respwidth+v_respwidth-1:i*v_respwidth] = _data;
          end
          if(|lv_tag_resp.ded == 1)
            wr_ded_tag_log <= tagged Valid ECC_dcache_tag{address: lv_evict_address, 
                                                         way: lv_tag_resp.ded & v_reg_valid[set_index]};

          if (|lv_data_resp.line_ded == 1)
            wr_ded_data_log <= tagged Valid ECC_dcache_data{address: lv_evict_address, 
                                                           banks: lv_data_resp.line_ded,
                                                           way : _way};
        `endif
          if(rg_release_readphase ) begin

            `logLevel( dcache, 0, $format("[%2d]DCACHE: Evicting Addr:%h set:%d tag:%h data:%h", id,lv_evict_address,set_index,tag,dataline))
            ff_mem_wr_request.enq(DCache_mem_writereq{address:lv_evict_address,
                                                  burst_len:fromInteger(((`dblocks * `dwords * 8) / `dbuswidth)  - 1 ),
                                                  burst_size:fromInteger(valueOf(TLog#(TDiv#(`dbuswidth,8)))),
                                                  data: truncateLSB(dataline)
                                              });
          `ifdef perfmonitors
            wr_total_evictions <= 1;
          `endif
          end
          // update the valid and dirty bits of the rams. Also release the fillbuffer entry
        `ifdef perfmonitors
          wr_total_fb_releases <= 1;
        `endif
          v_reg_valid[set_index][waynum]<=1;
          v_reg_dirty[set_index][waynum]<=lv_release_dirty;
          m_tag.ma_request(True,set_index,lv_release_addr,waynum);
          m_data.ma_request(True,set_index,lv_release_line,waynum,'1);
          m_fillbuffer.ma_perform_release;
          `logLevel( dcache, 0, $format("[%2d]DCACHE: Release: Upd Addr:%h set:%d way:%d dirty:%b data:%h", 
                id,lv_release_addr, set_index,waynum,lv_release_dirty, lv_release_line))
          if(rg_release_readphase || set_index == rg_recent_req )
            rg_performing_replay <= True;
     // ------------------ replacement policy updates -------------------------------------//
          Bool alldirty =  (&v_reg_valid[set_index] == 1 && &v_reg_dirty[set_index] == 1);
          Bool nonedirty = (&v_reg_valid[set_index] == 1 && |v_reg_dirty[set_index] == 0);
          Bool update_req = alldirty || nonedirty;

          if (`drepl == 0) begin // RANDOM
            replacement.update_set(set_index, waynum);
          end
          else if (`drepl == 1) begin // RR
            if (update_req) 
              replacement.update_set(set_index, waynum);
          end
          else if (`drepl == 2) begin // PLRU
            if (wr_ram_hitset matches tagged Valid .i &&& i == set_index) begin
              // Do not update on hit to same set
            end
            else
              replacement.update_set(set_index, waynum);
          end
          else if (`drepl == 3) begin // FIFO
            // Always advance FIFO pointer after replacement
            replacement.update_set(set_index, waynum);
          end
          else if (`drepl == 4) begin // LFU
            // Always increment counter for accessed way
            replacement.update_set(set_index, waynum);
          end
// -----------------------------------------------------------------------------------//
        end
      end
      else begin
        // enter here only if the fillbuffer entry has an error
        m_fillbuffer.ma_perform_release;
      end
    endrule

    /*doc:rule: */
    rule rl_commit_stores(m_storebuffer.mv_sb_head_commit && m_storebuffer.mv_sb_head_valid 
                                                                          && !sb_busy && !sb_empty);
      let sb_entry = m_storebuffer.mv_sb_head;
      `logLevel( dcache, 6, $format("[%2d]DCACHE: Committing store to Available line",id))
      m_fillbuffer.ma_from_storebuffer(sb_entry.mask, sb_entry.data, sb_entry.fbindex, sb_entry.addr);
      rg_globaldirty <= True;
      m_storebuffer.ma_increment_head();
    endrule:rl_commit_stores

    rule rl_initiate_io(m_iobuffer.mv_io_head_valid && !io_empty && !rg_io_busy);
      let io_entry = m_iobuffer.mv_io_head;
      rg_io_busy <= True;
      ff_mem_io_request.enq(DCache_io_req{address: io_entry.address, data: duplicate(io_entry.data),
                                      size: io_entry.size, read_write: io_entry.access == 1});
      `logLevel( dcache, 0, $format("DCACHE[%2d]: Initiating IO request: ",id,fshow(io_entry)))
    endrule:rl_initiate_io
  
  `ifdef atomic
    /*doc:func: This function carries out the atomic operations based on the RISC-V ISA spec*/
    function Bit#(TMul#(`dwords,8)) fn_atomic_io_op (Bit#(5) op,  Bit#(TMul#(`dwords,8)) rs2,  Bit#(TMul#(`dwords,8)) loaded);
      Bit#(TMul#(`dwords,8)) op1 = loaded;
      Bit#(TMul#(`dwords,8)) op2 = rs2;
    `ifdef RV64
      if(op[4]==0)begin
        op1=signExtend(loaded[31:0]);
        op2= signExtend(rs2[31:0]);
      end
    `endif
      Int#(TMul#(`dwords,8)) s_op1 = unpack(op1);
      Int#(TMul#(`dwords,8)) s_op2 = unpack(op2);

      case (op[3:0])
          'b0011:return op2;
          'b0000:return (op1+op2);
          'b0010:return (op1^op2);
          'b0110:return (op1&op2);
          'b0100:return (op1|op2);
          'b1100:return min(op1,op2);
          'b1110:return max(op1,op2);
          'b1000:return pack(min(s_op1,s_op2));
          'b1010:return pack(max(s_op1,s_op2));
          default:return op1;
        endcase
    endfunction
  `endif

    /*doc:rule: */
    rule rl_io_response(rg_io_busy);
      let io_entry = m_iobuffer.mv_io_head;
      let mem_response = ff_mem_io_resp.first();
      `logLevel( dcache, 0, $format("[%2d]DCACHE: IO Response from Bus",id,fshow(mem_response)))
	    Bit#(TLog#(TDiv#(`dbuswidth,8))) offset = truncate(io_entry.address);
	    mem_response.data = mem_response.data >> {offset,3'b0};
		  mem_response.data = case(io_entry.size)
		    'b000: signExtend(mem_response.data[7:0]);
		    'b001: signExtend(mem_response.data[15:0]);
		    'b010: signExtend(mem_response.data[31:0]);
  	  	'b100: zeroExtend(mem_response.data[7:0]);
  	  	'b101: zeroExtend(mem_response.data[15:0]);
  	  	'b110: zeroExtend(mem_response.data[31:0]);
  	  	default: mem_response.data;
		  endcase;
      ff_mem_io_resp.deq;
      Bit#(`causesize) lv_cause = io_entry.access == 0?`Load_access_fault:`Store_access_fault;
      let lv_response = DMem_core_response{word:mem_response.error? `ifdef supervisor truncate(io_entry.vaddr) `else  zeroExtend(io_entry.address)  `endif : 
                                         `ifdef atomic (io_entry.access == 2)? rg_atomic_rd_data: `endif truncate(mem_response.data), 
                                          trap: mem_response.error,
                                          entry_alloc: False,
                                          is_io: False, cause: lv_cause, epochs: io_entry.epoch};
    `ifdef supervisor 
      if (io_entry.is_ptw_req) begin
          ff_ptw_response.enq(lv_response);
          m_iobuffer.ma_increment_head;
          rg_io_busy <= False;
      end
      else
    `endif
    `ifdef atomic
      if (io_entry.access==2 && !rg_io_atomic_done && !mem_response.error) begin
        Bit#(`respwidth) lv_mem_response_data = truncate(mem_response.data);
        Bit#(`respwidth) lv_io_entry_data = truncate(io_entry.data);
        let _new_store = fn_atomic_io_op(io_entry.atomic_op, lv_io_entry_data, lv_mem_response_data);
        rg_io_atomic_done <= True;
        rg_atomic_rd_data <= truncate(mem_response.data);
        ff_mem_io_request.enq(DCache_io_req{address: io_entry.address, data: duplicate(_new_store),
                                      size: io_entry.size, read_write: True});
        `logLevel( dcache, 0, $format("DACCHE[%2d]: IO Atomic Rd phase Done. NewSt:%h",id, _new_store))
      end
      else if (io_entry.access == 2 && rg_io_atomic_done) begin
        rg_io_atomic_done <= False;
        m_iobuffer.ma_increment_head();
        rg_io_busy <= False;
        rg_core_io_response <= tagged Valid (lv_response);
        `logLevel( dcache, 0, $format("DACCHE[%2d]: IO Atomic Wr phase Done.",id))
      end
      else
    `endif
      begin
        rg_core_io_response <= tagged Valid  (lv_response);
        m_iobuffer.ma_increment_head;
        rg_io_busy <= False;
      end
    endrule:rl_io_response

    interface receive_core_req=interface Put
      method Action put(DCache_core_request#(`vaddr,`respwidth,`desize) req)
                        if( ff_core_response.notFull && !rg_fence_stall 
                                                     && !fb_full && !rg_performing_replay
                                                   && !sb_busy);
      `ifdef perfmonitors
          if(req.access == 0)
            wr_total_read_access <= 1;
          if(req.access == 1)
            wr_total_write_access <= 1;
        `ifdef atomic
          if(req.access == 2)
            wr_total_atomic_access <= 1;
        `endif
      `endif
        Bit#(`paddr) phyaddr = truncate(req.address);
        Bit#(`setbits) set_index=req.fence?0:phyaddr[v_setbits+v_blockbits+v_wordbits-1:v_blockbits+v_wordbits];
        `logLevel( dcache, 0, $format("[%2d]DCACHE: Request: ",id,fshow(req)))
        `logLevel( dcache, 0, $format("[%2d]DCACHE: set:%d",id,set_index))
        ff_core_request.enq(req);
        rg_fence_stall<=req.fence;
        rg_recent_req <= set_index;
        if(wr_cache_enable) begin
          m_tag.ma_request(False, set_index, lv_release_addr, ?);
          m_data.ma_request(False, set_index, lv_release_line, ?, '1);
        end
        wr_takingrequest <= True;
      endmethod
    endinterface;
    method Action ma_commit_store(Tuple2#(Bit#(`desize), Bit#(TLog#(`dsbsize))) storecommit);
      let {currepoch, sbid} = storecommit;
    `ifdef ASSERT
      dynamicAssert(m_storebuffer.mv_sb_head_valid,"SB Commit to invalid Entry");
    `endif
      let sb_entry = m_storebuffer.mv_sb_head;
      `logLevel( dcache, 6, $format("[%2d]DCACHE: Commit Store entry:",id,fshow(sb_entry)))
      if(sb_entry.epoch == currepoch) begin
        m_storebuffer.ma_commit_store(sbid);
      end
      else begin
        `logLevel( dcache, 0, $format("[%2d]DCACHE: Store is being dropped- epoch mismatch",id))
        m_storebuffer.ma_increment_head;
      end

    endmethod
    method Action ma_commit_io(Bit#(`desize) currepoch);
    `ifdef ASSERT
      dynamicAssert(!m_iobuffer.mv_io_head_valid,"IO Head is already ready to commit.");
    `endif
      let io_entry = m_iobuffer.mv_io_head;
      `logLevel( dcache, 6, $format("[%2d]DCACHE: Commit IO entry:",id,fshow(io_entry)))
      if(io_entry.epoch == currepoch) begin
        m_iobuffer.ma_commit_io();
      end
      else begin
        `logLevel( dcache, 0, $format("[%2d]DCACHE: IO is being dropped- epoch mismatch",id))
        m_iobuffer.ma_increment_head;
      end

    endmethod

    method Action ma_cache_enable(Bool c);
      wr_cache_enable <= c;
    endmethod
    method Action ma_curr_priv (Bit#(2) c);
      wr_priv <= c;
    endmethod

    interface send_mem_rd_req = toGet(ff_mem_rd_request);
    interface receive_mem_rd_resp = toPut(ff_mem_rd_resp);

    interface send_core_cache_resp = toGet(ff_core_response);
    method send_core_io_resp = rg_core_io_response;

    interface send_mem_io_req = toGet(ff_mem_io_request);
    interface receive_mem_io_resp = toPut(ff_mem_io_resp);

    method send_mem_wr_req = ff_mem_wr_request.first;
    method Action deq_mem_wr_req;
      ff_mem_wr_request.deq;
    endmethod
    interface receive_mem_wr_resp = toPut(ff_mem_wr_resp);
  `ifdef supervisor
    interface get_ptw_resp = toGet(ff_ptw_response);
    interface put_pa_from_tlb = toPut(ff_from_tlb);
    interface get_hold_req = toGet(ff_hold_request);
  `endif
    `ifdef perfmonitors
      method mv_perf_counters = {wr_total_read_access , wr_total_write_access , wr_total_atomic_access
                            , wr_total_io_reads , wr_total_io_writes , wr_total_read_miss ,
                              wr_total_write_miss , wr_total_atomic_miss , wr_total_read_fb_hits,
                              wr_total_write_fb_hits, wr_total_atomic_fb_hits,
                              wr_total_fb_releases, wr_total_evictions };
    `endif
    method mv_storebuffer_empty = sb_empty;
    method mv_cache_available = ff_core_response.notFull && ff_core_request.notFull &&
        !rg_fence_stall && !fb_full && !rg_performing_replay && !sb_full &&
        !sb_busy && !io_full `ifdef dcache_ecc && !rg_perform_sec && !rg_halt_ram_check `endif ;
  `ifdef dcache_ecc
    method mv_ded_data = wr_ded_data_log;
    method mv_sed_data = wr_sed_data_log;
    method mv_ded_tag = wr_ded_tag_log;
    method mv_sed_tag = wr_ded_tag_log;
    method Action ma_ram_request(DRamAccess access)if(!rg_fence_stall && !rg_performing_replay);
      Bit#(blocksize) _banks = 0;
      _banks[access.banks] = 1;
      if(!access.tag_data) begin // access tag;
        m_tag.ma_request(access.read_write, access.index, truncate(access.data), access.way);
      end
      else begin
        m_data.ma_request(access.read_write, access.index, duplicate(access.data) , access.way,
        _banks);
      end
      rg_access_req <= tagged Valid access;
    endmethod
    method Bit#(`respwidth) mv_ram_response if(!rg_fence_stall &&& !rg_performing_replay 
                                              &&& rg_access_req matches tagged Valid .access);
      Bit#(`respwidth) tag_response = zeroExtend(m_tag.mv_sideband_read(access.way));
      Bit#(`respwidth) data_response = m_data.mv_sideband_read(access.way,
                                        access.banks);
      if(!access.tag_data) // access tag
        return tag_response;
      else
        return data_response;
    endmethod
  `endif
  endmodule: mkdcache

endpackage

