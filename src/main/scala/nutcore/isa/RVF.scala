package nutcore

import chisel3._
import chisel3.util._

object RVFInstr extends HasInstrType {
  //F Extension
  def FLW       = BitPat("b????????????_?????_010_?????_0000111")
  def FSW       = BitPat("b???????_?????_?????_010_?????_0100111")
  def FMADD_S   = BitPat("b?????_00_?????_?????_???_?????_1000011")
  def FMSUB_S   = BitPat("b?????_00_?????_?????_???_?????_1000111")
  def FNMSUB_S  = BitPat("b?????_00_?????_?????_???_?????_1001011")
  def FNMADD_S  = BitPat("b?????_00_?????_?????_???_?????_1001111")
  def FADD_S    = BitPat("b0000000_?????_?????_???_?????_1010011")
  def FSUB_S    = BitPat("b0000100_?????_?????_???_?????_1010011")
  def FMUL_S    = BitPat("b0001000_?????_?????_???_?????_1010011")
  def FDIV_S    = BitPat("b0001100_?????_?????_???_?????_1010011")
  def FSQRT_S   = BitPat("b0101100_00000_?????_???_?????_1010011")
  def FSGNJ_S   = BitPat("b0010000_?????_?????_000_?????_1010011")
  def FSGNJN_S  = BitPat("b0010000_?????_?????_001_?????_1010011")
  def FSGNJX_S  = BitPat("b0010000_?????_?????_010_?????_1010011")
  def FMIN_S    = BitPat("b0010100_?????_?????_000_?????_1010011")
  def FMAX_S    = BitPat("b0010100_?????_?????_001_?????_1010011")
  def FCVT_W_S  = BitPat("b1100000_00000_?????_???_?????_1010011")
  def FCVT_WU_S = BitPat("b1100000_00001_?????_???_?????_1010011")
  def FMV_X_W   = BitPat("b1110000_00000_?????_000_?????_1010011")
  def FEQ_S     = BitPat("b1010000_?????_?????_010_?????_1010011")
  def FLT_S     = BitPat("b1010000_?????_?????_001_?????_1010011")
  def FLE_S     = BitPat("b1010000_?????_?????_000_?????_1010011")
  def FCLASS_S  = BitPat("b1110000_00000_?????_001_?????_1010011")
  def FCVT_S_W  = BitPat("b1101000_00000_?????_???_?????_1010011")
  def FCVT_S_WU = BitPat("b1101000_00001_?????_???_?????_1010011")
  def FMV_W_X   = BitPat("b1111000_00000_?????_000_?????_1010011")
  def FCVT_L_S  = BitPat("b1100000_00010_?????_???_?????_1010011")
  def FCVT_LU_S = BitPat("b1100000_00011_?????_???_?????_1010011")
  def FCVT_S_L  = BitPat("b1101000_00010_?????_???_?????_1010011")
  def FCVT_S_LU = BitPat("b1101000_00011_?????_???_?????_1010011")

  //D Extension
  def FLD       = BitPat("b????????????_?????_011_?????_0000111")
  def FSD       = BitPat("b???????_?????_?????_011_?????_0100111")
  def FMADD_D   = BitPat("b?????_01_?????_?????_???_?????_1000011")
  def FMSUB_D   = BitPat("b?????_01_?????_?????_???_?????_1000111")
  def FNMSUB_D  = BitPat("b?????_01_?????_?????_???_?????_1001011")
  def FNMADD_D  = BitPat("b?????_01_?????_?????_???_?????_1001111")
  def FADD_D    = BitPat("b0000001_?????_?????_???_?????_1010011")
  def FSUB_D    = BitPat("b0000101_?????_?????_???_?????_1010011")
  def FMUL_D    = BitPat("b0001001_?????_?????_???_?????_1010011")
  def FDIV_D    = BitPat("b0001101_?????_?????_???_?????_1010011")
  def FSQRT_D   = BitPat("b0101101_00000_?????_???_?????_1010011")
  def FSGNJ_D   = BitPat("b0010001_?????_?????_000_?????_1010011")
  def FSGNJN_D  = BitPat("b0010001_?????_?????_001_?????_1010011")
  def FSGNJX_D  = BitPat("b0010001_?????_?????_010_?????_1010011")
  def FMIN_D    = BitPat("b0010101_?????_?????_000_?????_1010011")
  def FMAX_D    = BitPat("b0010101_?????_?????_001_?????_1010011")
  def FCVT_S_D  = BitPat("b0100000_00001_?????_???_?????_1010011")
  def FCVT_D_S  = BitPat("b0100001_00000_?????_???_?????_1010011")
  def FEQ_D     = BitPat("b1010001_?????_?????_010_?????_1010011")
  def FLT_D     = BitPat("b1010001_?????_?????_001_?????_1010011")
  def FLE_D     = BitPat("b1010001_?????_?????_000_?????_1010011")
  def FCLASS_D  = BitPat("b1110001_00000_?????_001_?????_1010011")
  def FCVT_W_D  = BitPat("b1100001_00000_?????_???_?????_1010011")
  def FCVT_WU_D = BitPat("b1100001_00001_?????_???_?????_1010011")
  def FCVT_D_W  = BitPat("b1101001_00000_?????_???_?????_1010011")
  def FCVT_D_WU = BitPat("b1101001_00001_?????_???_?????_1010011")
  def FCVT_L_D  = BitPat("b1100001_00010_?????_???_?????_1010011")
  def FCVT_LU_D = BitPat("b1100001_00011_?????_???_?????_1010011")
  def FMV_X_D   = BitPat("b1110001_00000_?????_000_?????_1010011")
  def FCVT_D_L  = BitPat("b1101001_00010_?????_???_?????_1010011")
  def FCVT_D_LU = BitPat("b1101001_00011_?????_???_?????_1010011")
  def FMV_D_X   = BitPat("b1111001_00000_?????_000_?????_1010011")
                                                             
  val table = Array(                                         
    FLW       -> List(InstrI, FuType.lsu, LSUOpType.lw        ),        //i->f
    FSW       -> List(InstrS, FuType.lsu, LSUOpType.sw        ),                      //i,f->M
    FMADD_S   -> List(InstrR4, FuType.fma, FPUOpType.fmadd_s ),//f->f
    FMSUB_S   -> List(InstrR4, FuType.fma, FPUOpType.fmsub_s ),//f->f
    FNMSUB_S  -> List(InstrR4, FuType.fma, FPUOpType.fnmsub_s),//f->f
    FNMADD_S  -> List(InstrR4, FuType.fma, FPUOpType.fnmadd_s),//f->f
    FADD_S    -> List(InstrR, FuType.fma, FPUOpType.fadd_s   ),//f->f
    FSUB_S    -> List(InstrR, FuType.fma, FPUOpType.fsub_s   ),//f->f
    FMUL_S    -> List(InstrR, FuType.fma, FPUOpType.fmul_s   ),//f->f
    FDIV_S    -> List(InstrR, FuType.fdivsqrt, FPUOpType.fdiv_s),//f->f
    FSQRT_S   -> List(InstrR, FuType.fdivsqrt, FPUOpType.fsqrt_s),//f->f
    FSGNJ_S   -> List(InstrR, FuType.fconv, FPUOpType.fsgnj_s  ),//f->f
    FSGNJN_S  -> List(InstrR, FuType.fconv, FPUOpType.fsgnjn_s ),//f->f
    FSGNJX_S  -> List(InstrR, FuType.fconv, FPUOpType.fsgnjx_s ),//f->f
    FMIN_S    -> List(InstrR, FuType.fcomp, FPUOpType.fmin_s   ),//f->f
    FMAX_S    -> List(InstrR, FuType.fcomp, FPUOpType.fmax_s   ),//f->f
    FCVT_W_S  -> List(InstrI, FuType.fconv, FPUOpType.fcvt_w_s ),              //f->i
    FCVT_WU_S -> List(InstrI, FuType.fconv, FPUOpType.fcvt_wu_s),              //f->i
    FMV_X_W   -> List(InstrI, FuType.fconv, FPUOpType.fmv_x_w  ),              //f->i
    FEQ_S     -> List(InstrR, FuType.fcomp, FPUOpType.feq_s    ),              //f->i
    FLT_S     -> List(InstrR, FuType.fcomp, FPUOpType.flt_s    ),              //f->i
    FLE_S     -> List(InstrR, FuType.fcomp, FPUOpType.fle_s    ),              //f->i
    FCLASS_S  -> List(InstrI, FuType.fconv, FPUOpType.fclass_s ),              //f->i
    FCVT_S_W  -> List(InstrI, FuType.fconv, FPUOpType.fcvt_s_w ),       //i->f
    FCVT_S_WU -> List(InstrI, FuType.fconv, FPUOpType.fcvt_s_wu),       //i->f
    FMV_W_X   -> List(InstrI, FuType.fconv, FPUOpType.fmv_w_x  ),       //i->f
    FCVT_L_S  -> List(InstrI, FuType.fconv, FPUOpType.fcvt_l_s ),              //f->i
    FCVT_LU_S -> List(InstrI, FuType.fconv, FPUOpType.fcvt_lu_s),              //f->i
    FCVT_S_L  -> List(InstrI, FuType.fconv, FPUOpType.fcvt_s_l ),       //i->f
    FCVT_S_LU -> List(InstrI, FuType.fconv, FPUOpType.fcvt_s_lu),       //i->f

    FLD       -> List(InstrI, FuType.lsu, LSUOpType.ld       ),        //i->f
    FSD       -> List(InstrS, FuType.lsu, LSUOpType.sd       ),                      //i,f->M
    FMADD_D   -> List(InstrR4, FuType.fma, FPUOpType.fmadd_d ),//f->f
    FMSUB_D   -> List(InstrR4, FuType.fma, FPUOpType.fmsub_d ),//f->f
    FNMSUB_D  -> List(InstrR4, FuType.fma, FPUOpType.fnmsub_d),//f->f
    FNMADD_D  -> List(InstrR4, FuType.fma, FPUOpType.fnmadd_d),//f->f
    FADD_D    -> List(InstrR, FuType.fma, FPUOpType.fadd_d   ),//f->f
    FSUB_D    -> List(InstrR, FuType.fma, FPUOpType.fsub_d   ),//f->f
    FMUL_D    -> List(InstrR, FuType.fma, FPUOpType.fmul_d   ),//f->f
    FDIV_D    -> List(InstrR, FuType.fdivsqrt, FPUOpType.fdiv_d),//f->f
    FSQRT_D   -> List(InstrR, FuType.fdivsqrt, FPUOpType.fsqrt_d),//f->f
    FSGNJ_D   -> List(InstrR, FuType.fconv, FPUOpType.fsgnj_d  ),//f->f
    FSGNJN_D  -> List(InstrR, FuType.fconv, FPUOpType.fsgnjn_d ),//f->f
    FSGNJX_D  -> List(InstrR, FuType.fconv, FPUOpType.fsgnjx_d ),//f->f
    FMIN_D    -> List(InstrR, FuType.fcomp, FPUOpType.fmin_d   ),//f->f
    FMAX_D    -> List(InstrR, FuType.fcomp, FPUOpType.fmax_d   ),//f->f
    FCVT_S_D  -> List(InstrI, FuType.fconv, FPUOpType.fcvt_s_d ),
    FCVT_D_S  -> List(InstrI, FuType.fconv, FPUOpType.fcvt_d_s),
    FEQ_D     -> List(InstrR, FuType.fcomp, FPUOpType.feq_d    ),
    FLT_D     -> List(InstrR, FuType.fcomp, FPUOpType.flt_d    ),
    FLE_D     -> List(InstrR, FuType.fcomp, FPUOpType.fle_d    ),
    FCLASS_D  -> List(InstrI, FuType.fconv, FPUOpType.fclass_d ),
    FCVT_W_D  -> List(InstrI, FuType.fconv, FPUOpType.fcvt_w_d ),       //i->f
    FCVT_WU_D -> List(InstrI, FuType.fconv, FPUOpType.fcvt_wu_d),       //i->f
    FCVT_D_W  -> List(InstrI, FuType.fconv, FPUOpType.fcvt_d_w  ),       //i->f
    FCVT_D_WU -> List(InstrI, FuType.fconv, FPUOpType.fcvt_d_wu ),              //f->i
    FCVT_L_D  -> List(InstrI, FuType.fconv, FPUOpType.fcvt_l_d),              //f->i
    FCVT_LU_D -> List(InstrI, FuType.fconv, FPUOpType.fcvt_lu_d ),       //i->f
    FMV_X_D   -> List(InstrI, FuType.fconv, FPUOpType.fmv_x_d),       //i->f
    FCVT_D_L  -> List(InstrI, FuType.fconv, FPUOpType.fcvt_d_l),
    FCVT_D_LU -> List(InstrI, FuType.fconv, FPUOpType.fcvt_d_lu ),
    FMV_D_X   -> List(InstrI, FuType.fconv, FPUOpType.fmv_d_x)
  )

  val FExtraTable = Array(
//                     Sr1_fpRen
//                     |  Sr2_fpRen
//                     |  |  Sr3_fpRen
//                     |  |  |  fpWen
//                     |  |  |  |
    FLW       -> List(N, N, N, Y),          //i->f
    FSW       -> List(N, Y, N, N),                      //i,f->M
    FMADD_S   -> List(Y, Y, Y, Y),//f->f
    FMSUB_S   -> List(Y, Y, Y, Y),//f->f
    FNMSUB_S  -> List(Y, Y, Y, Y),//f->f
    FNMADD_S  -> List(Y, Y, Y, Y),//f->f
    FADD_S    -> List(Y, Y, N, Y),//f->f
    FSUB_S    -> List(Y, Y, N, Y),//f->f
    FMUL_S    -> List(Y, Y, N, Y),//f->f
    FDIV_S    -> List(Y, Y, N, Y),//f->f
    FSQRT_S   -> List(Y, N, N, Y),//f->f
    FSGNJ_S   -> List(Y, Y, N, Y),//f->f
    FSGNJN_S  -> List(Y, Y, N, Y),//f->f
    FSGNJX_S  -> List(Y, Y, N, Y),//f->f
    FMIN_S    -> List(Y, Y, N, Y),//f->f
    FMAX_S    -> List(Y, Y, N, Y),//f->f
    FCVT_W_S  -> List(Y, N, N, N),              //f->i
    FCVT_WU_S -> List(Y, N, N, N),              //f->i
    FMV_X_W   -> List(Y, N, N, N),              //f->i
    FEQ_S     -> List(Y, Y, N, N),              //f->i
    FLT_S     -> List(Y, Y, N, N),              //f->i
    FLE_S     -> List(Y, Y, N, N),              //f->i
    FCLASS_S  -> List(Y, N, N, N),              //f->i
    FCVT_S_W  -> List(N, N, N, Y),       //i->f
    FCVT_S_WU -> List(N, N, N, Y),       //i->f
    FMV_W_X   -> List(N, N, N, Y),       //i->f
    FCVT_L_S  -> List(Y, N, N, N),              //f->i
    FCVT_LU_S -> List(Y, N, N, N),              //f->i
    FCVT_S_L  -> List(N, N, N, Y),       //i->f
    FCVT_S_LU -> List(N, N, N, Y),       //i->f

    FLD       -> List(N, N, N, Y),          //i->f
    FSD       -> List(N, Y, N, N),                      //i,f->M
    FMADD_D   -> List(Y, Y, Y, Y),//f->f
    FMSUB_D   -> List(Y, Y, Y, Y),//f->f
    FNMSUB_D  -> List(Y, Y, Y, Y),//f->f
    FNMADD_D  -> List(Y, Y, Y, Y),//f->f
    FADD_D    -> List(Y, Y, N, Y),//f->f
    FSUB_D    -> List(Y, Y, N, Y),//f->f
    FMUL_D    -> List(Y, Y, N, Y),//f->f
    FDIV_D    -> List(Y, Y, N, Y),//f->f
    FSQRT_D   -> List(Y, N, N, Y),//f->f
    FSGNJ_D   -> List(Y, Y, N, Y),//f->f
    FSGNJN_D  -> List(Y, Y, N, Y),//f->f
    FSGNJX_D  -> List(Y, Y, N, Y),//f->f
    FMIN_D    -> List(Y, Y, N, Y),//f->f
    FMAX_D    -> List(Y, Y, N, Y),//f->f
    FCVT_S_D  -> List(Y, N, N, Y),
    FCVT_D_S  -> List(Y, N, N, Y),
    FEQ_D     -> List(Y, Y, N, N),
    FLT_D     -> List(Y, Y, N, N),
    FLE_D     -> List(Y, Y, N, N),
    FCLASS_D  -> List(Y, N, N, N),
    FCVT_W_D  -> List(Y, N, N, N),
    FCVT_WU_D -> List(Y, N, N, N),
    FCVT_D_W  -> List(N, N, N, Y),
    FCVT_D_WU -> List(N, N, N, Y),
    FCVT_L_D  -> List(Y, N, N, N),
    FCVT_LU_D -> List(Y, N, N, N),
    FMV_X_D   -> List(Y, N, N, N),
    FCVT_D_L  -> List(N, N, N, Y),
    FCVT_D_LU -> List(N, N, N, Y),
    FMV_D_X   -> List(N, N, N, Y)
  
  )

}