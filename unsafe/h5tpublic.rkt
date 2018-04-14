#lang racket

;; Racket Foreign interface
(require ffi/unsafe
         ffi/unsafe/define
         ffi/winapi
         rackunit
         "h5-utilities.rkt"
         "h5public.rkt"
         "h5ipublic.rkt") ;; ids

(provide (all-defined-out))

;; #define HOFFSET(S,M)    (offsetof(S,M))

;; These are the various classes of datatypes 
;; If this goes over 16 types (0-15), the file format will need to change)
(define H5T_class_t
  (_enum
   '(
     H5T_NO_CLASS  = -1 ;; error
     H5T_INTEGER   =  0 ;; integer types
     H5T_FLOAT     =  1 ;; floating-point types
     H5T_TIME      =  2 ;; date and time types
     H5T_STRING    =  3 ;; character string types
     H5T_BITFIELD  =  4 ;; bit field types
     H5T_OPAQUE    =  5 ;; opaque types
     H5T_COMPOUND  =  6 ;; compound types
     H5T_REFERENCE =  7 ;; reference types
     H5T_ENUM      =  8 ;; enumeration types
     H5T_VLEN      =  9 ;; Variable-Length types
     H5T_ARRAY     = 10 ;; Array types
     H5T_NCLASSES       ;; this must be last
     )))

;; Byte orders
(define H5T_order_t
  (_enum
   '(
     H5T_ORDER_ERROR = -1 ;; error
     H5T_ORDER_LE     = 0 ;; little endian
     H5T_ORDER_BE     = 1 ;; bit endian
     H5T_ORDER_VAX    = 2 ;; MAX mixed endian
     H5T_ORDER_MIXED  = 3 ;; compound type with mixed member orders
     H5T_ORDER_NONE   = 4 ;; no particular order (strings bits..)
                          ;; 5T_ORDER_NONE must be last
     )))

;; Types of integer sign schemes 
(define H5T_sign_t
  (_enum
   '(
     H5T_SGN_ERROR        = -1  ;; rror                                      
     H5T_SGN_NONE         = 0   ;; his is an unsigned type                   
     H5T_SGN_2            = 1   ;; wo's complement                           
     H5T_NSGN             = 2    ;; his must be last!                         
     )))

;; Floating-point normalization schemes
(define H5T_norm_t
  (_enum
   '(
     H5T_NORM_ERROR       = -1 ;; rror                                      
     H5T_NORM_IMPLIED     =  0 ;; sb of mantissa isn't stored always 1     
     H5T_NORM_MSBSET      =  1 ;; sb of mantissa is always 1                
     H5T_NORM_NONE        =  2 ;; ot normalized                             
     ;; 5T_NORM_NONE must be last 
     )))


#|
* Character set to use for text strings.Do not change these values since
* they appear in HDF5 files!
|#
(define H5T_cset_t
  (_enum
   '(
     H5T_CSET_ERROR = -1 #|error|#
     H5T_CSET_ASCII = 0 #|US ASCII |#
     H5T_CSET_UTF8 = 1 #|UTF-8 Unicode encoding		 |#
     H5T_CSET_RESERVED_2 = 2 #|reserved for later use		 |#
     H5T_CSET_RESERVED_3 = 3 #|reserved for later use		 |#
     H5T_CSET_RESERVED_4 = 4 #|reserved for later use		 |#
     H5T_CSET_RESERVED_5 = 5 #|reserved for later use		 |#
     H5T_CSET_RESERVED_6 = 6 #|reserved for later use		 |#
     H5T_CSET_RESERVED_7 = 7 #|reserved for later use		 |#
     H5T_CSET_RESERVED_8 = 8 #|reserved for later use		 |#
     H5T_CSET_RESERVED_9 = 9 #|reserved for later use		 |#
     H5T_CSET_RESERVED_10 = 10 #|reserved for later use		 |#
     H5T_CSET_RESERVED_11 = 11 #|reserved for later use		 |#
     H5T_CSET_RESERVED_12 = 12 #|reserved for later use		 |#
     H5T_CSET_RESERVED_13 = 13 #|reserved for later use		 |#
     H5T_CSET_RESERVED_14 = 14 #|reserved for later use		 |#
     H5T_CSET_RESERVED_15 = 15 #|reserved for later use		 |#
     )))
(define H5T_NCSET 'H5T_CSET_RESERVED_2)		#|Number of character sets actually defined|#

#|
* Type of padding to use in character strings.Do not change these values
* since they appear in HDF5 files!
|#
(define H5T_str_t
  (_enum
   '(
     H5T_STR_ERROR = -1 #|error|#
     H5T_STR_NULLTERM = 0 #|null terminate like in C |#
     H5T_STR_NULLPAD = 1 #|pad with nulls |#
     H5T_STR_SPACEPAD = 2 #|pad with spaces like in Fortran|#
     H5T_STR_RESERVED_3 = 3 #|reserved for later use		 |#
     H5T_STR_RESERVED_4 = 4 #|reserved for later use		 |#
     H5T_STR_RESERVED_5 = 5 #|reserved for later use		 |#
     H5T_STR_RESERVED_6 = 6 #|reserved for later use		 |#
     H5T_STR_RESERVED_7 = 7 #|reserved for later use		 |#
     H5T_STR_RESERVED_8 = 8 #|reserved for later use		 |#
     H5T_STR_RESERVED_9 = 9 #|reserved for later use		 |#
     H5T_STR_RESERVED_10 = 10 #|reserved for later use		 |#
     H5T_STR_RESERVED_11 = 11 #|reserved for later use		 |#
     H5T_STR_RESERVED_12 = 12 #|reserved for later use		 |#
     H5T_STR_RESERVED_13 = 13 #|reserved for later use		 |#
     H5T_STR_RESERVED_14 = 14 #|reserved for later use		 |#
     H5T_STR_RESERVED_15 = 15 #|reserved for later use		 |#
     )))

(define H5T_NSTR 'H5T_STR_RESERVED_3) ;; num H5T_str_t types actually defined
#| Type of padding to use in other atomic types |#
(define H5T_pad_t
  (_enum
   '(
     H5T_PAD_ERROR = -1 #|error|#
     H5T_PAD_ZERO = 0 #|always set to zero |#
     H5T_PAD_ONE = 1 #|always set to one|#
     H5T_PAD_BACKGROUND = 2 #|set to background value|#

     H5T_NPAD = 3 #|THIS MUST BE LAST|#
     )))

#| Commands sent to conversion functions |#
(define H5T_cmd_t
  (_enum
   '(
     H5T_CONV_INIT	= 0	#|query and/or initialize private data	 |#
     H5T_CONV_CONV	= 1 	#|convert data from source to dest datatype |#
     H5T_CONV_FREE	= 2	#|function is being removed from path	 |#
     )))

#| How is the `bkg' buffer used by the conversion function? |#
(define H5T_bkg_t
  (_enum
   '(
     H5T_BKG_NO		= 0 	#|background buffer is not needed send NULL |#
     H5T_BKG_TEMP	= 1	#|bkg buffer used as temp storage only |#
     H5T_BKG_YES		= 2	#|init bkg buf with data before conversion |#
     )))

;; Type conversion client data
(define-cstruct _H5T_cdata_t
  ([command H5T_cmd_t]  ;; what should the conversion function do?
   [need_bkg H5T_bkg_t] ;; is the background buffer needed?
   [recalc hbool_t]     ;; recalculate private data
   [priv _pointer]))    ;; private data


#| Conversion function persistence |#
(define H5T_pers_t
  (_enum
   '(
     H5T_PERS_DONTCARE	= -1 	#|wild card				 |#
     H5T_PERS_HARD	= 0	#|hard conversion function		 |#
     H5T_PERS_SOFT	= 1 	#|soft conversion function		 |#
     )))

#| The order to retrieve atomic native datatype |#
(define H5T_direction_t
  (_enum
   '(
     H5T_DIR_DEFAULT = 0 #|default direction is inscendent|#
     H5T_DIR_ASCEND = 1 #|in inscendent order|#
     H5T_DIR_DESCEND = 2 #|in descendent order|#
     )))

#| The exception type passed into the conversion callback function |#
(define H5T_conv_except_t
  (_enum
   '(
     H5T_CONV_EXCEPT_RANGE_HI = 0 #|source value is greater than destination's range |#
     H5T_CONV_EXCEPT_RANGE_LOW = 1 #|source value is less than destination's range|#
     H5T_CONV_EXCEPT_PRECISION = 2 #|source value loses precision in destination|#
     H5T_CONV_EXCEPT_TRUNCATE = 3 #|source value is truncated in destination |#
     H5T_CONV_EXCEPT_PINF = 4 #|source value is positive infinity(floating number) |#
     H5T_CONV_EXCEPT_NINF = 5 #|source value is negative infinity(floating number) |#
     H5T_CONV_EXCEPT_NAN = 6 #|source value is NaN(floating number) |#
     )))

#| The return value from conversion callback function H5T_conv_except_func_t |#
(define H5T_conv_ret_t
  (_enum
   '(
     H5T_CONV_ABORT = -1 #|abort conversion |#
     H5T_CONV_UNHANDLED = 0 #|callback function failed to handle the exception|#
     H5T_CONV_HANDLED = 1 #|callback function handled the exception successfully|#
     )))


;; Variable Length Datatype struct in memory
;; (This is only used for VL sequences, not VL strings, which are stored in char *'s)
(define-cstruct _hvl_t
  ([len _size]    ;; Length of VL data (in base type units)
   [p _pointer])) ;; Pointer to VL data

;; Variable Length String information 
(define H5T_VARIABLE    (cast -1 _int64 _size))  ;; Indicate that a string is variable length (null-terminated in C, instead of fixed length)

;; Opaque information
(define H5T_OPAQUE_TAG_MAX      256)    ;; Maximum length of an opaque tag
;; This could be raised without too much difficulty


;; All datatype conversion functions are...
(define H5T_conv_t
  (_fun (src_id : hid_t)
         (dst_id : hid_t)
         (cdata : _H5T_cdata_t)
         (nelmts : _size)
         (buf_stride : _size)
         (bkg_stride : _size)
         (buf : _pointer)
         (bkg : _pointer)
         (dset_xfer_plist : hid_t)
         -> herr_t))

#| Exception handler.  If an exception like overflow happenes during conversion,
 * this function is called if it's registered through H5Pset_type_conv_cb.
|#
(define H5T_conv_except_func_t
  (_fun (except_type : H5T_conv_except_t)
         (src_id : hid_t)
         (dst_id : hid_t)
         (src_buf : _pointer)
         (dst_buf : _pointer)
         (user_data : _pointer)
         -> H5T_conv_ret_t))

#|
 * The IEEE floating point types in various byte orders.
|#
(local () (H5open)(void))
(define-c H5T_IEEE_F32BE_g hdf5-lib hid_t)
(define-c H5T_IEEE_F32LE_g hdf5-lib hid_t)
(define-c H5T_IEEE_F64BE_g hdf5-lib hid_t)
(define-c H5T_IEEE_F64LE_g hdf5-lib hid_t)

(define H5T_IEEE_F32BE H5T_IEEE_F32BE_g)
(define H5T_IEEE_F32LE H5T_IEEE_F32LE_g)
(define H5T_IEEE_F64BE H5T_IEEE_F64BE_g)
(define H5T_IEEE_F64LE H5T_IEEE_F64LE_g)


#|
 * These are "standard" types.  For instance, signed (2's complement) and
 * unsigned integers of various sizes and byte orders.
|#
(define-c H5T_STD_I8BE_g hdf5-lib hid_t)
(define-c H5T_STD_I8LE_g hdf5-lib hid_t)
(define-c H5T_STD_I16BE_g hdf5-lib hid_t)
(define-c H5T_STD_I16LE_g hdf5-lib hid_t)
(define-c H5T_STD_I32BE_g hdf5-lib hid_t)
(define-c H5T_STD_I32LE_g hdf5-lib hid_t)
(define-c H5T_STD_I64BE_g hdf5-lib hid_t)
(define-c H5T_STD_I64LE_g hdf5-lib hid_t)
(define-c H5T_STD_U8BE_g hdf5-lib hid_t)
(define-c H5T_STD_U8LE_g hdf5-lib hid_t)
(define-c H5T_STD_U16BE_g hdf5-lib hid_t)
(define-c H5T_STD_U16LE_g hdf5-lib hid_t)
(define-c H5T_STD_U32BE_g hdf5-lib hid_t)
(define-c H5T_STD_U32LE_g hdf5-lib hid_t)
(define-c H5T_STD_U64BE_g hdf5-lib hid_t)
(define-c H5T_STD_U64LE_g hdf5-lib hid_t)
(define-c H5T_STD_B8BE_g hdf5-lib hid_t)
(define-c H5T_STD_B8LE_g hdf5-lib hid_t)
(define-c H5T_STD_B16BE_g hdf5-lib hid_t)
(define-c H5T_STD_B16LE_g hdf5-lib hid_t)
(define-c H5T_STD_B32BE_g hdf5-lib hid_t)
(define-c H5T_STD_B32LE_g hdf5-lib hid_t)
(define-c H5T_STD_B64BE_g hdf5-lib hid_t)
(define-c H5T_STD_B64LE_g hdf5-lib hid_t)
(define-c H5T_STD_REF_OBJ_g hdf5-lib hid_t)
(define-c H5T_STD_REF_DSETREG_g hdf5-lib hid_t)
(define H5T_STD_I8BE H5T_STD_I8BE_g)
(define H5T_STD_I8LE H5T_STD_I8LE_g)
(define H5T_STD_I16BE H5T_STD_I16BE_g)
(define H5T_STD_I16LE H5T_STD_I16LE_g)
(define H5T_STD_I32BE H5T_STD_I32BE_g)
(define H5T_STD_I32LE H5T_STD_I32LE_g)
(define H5T_STD_I64BE H5T_STD_I64BE_g)
(define H5T_STD_I64LE H5T_STD_I64LE_g)
(define H5T_STD_U8BE H5T_STD_U8BE_g)
(define H5T_STD_U8LE H5T_STD_U8LE_g)
(define H5T_STD_U16BE H5T_STD_U16BE_g)
(define H5T_STD_U16LE H5T_STD_U16LE_g)
(define H5T_STD_U32BE H5T_STD_U32BE_g)
(define H5T_STD_U32LE H5T_STD_U32LE_g)
(define H5T_STD_U64BE H5T_STD_U64BE_g)
(define H5T_STD_U64LE H5T_STD_U64LE_g)
(define H5T_STD_B8BE H5T_STD_B8BE_g)
(define H5T_STD_B8LE H5T_STD_B8LE_g)
(define H5T_STD_B16BE H5T_STD_B16BE_g)
(define H5T_STD_B16LE H5T_STD_B16LE_g)
(define H5T_STD_B32BE H5T_STD_B32BE_g)
(define H5T_STD_B32LE H5T_STD_B32LE_g)
(define H5T_STD_B64BE H5T_STD_B64BE_g)
(define H5T_STD_B64LE H5T_STD_B64LE_g)
(define H5T_STD_REF_OBJ H5T_STD_REF_OBJ_g)
(define H5T_STD_REF_DSETREG H5T_STD_REF_DSETREG_g)



#|
 * Types which are particular to Unix.
|#
(define-c H5T_UNIX_D32BE_g hdf5-lib hid_t)
(define-c H5T_UNIX_D32LE_g hdf5-lib hid_t)
(define-c H5T_UNIX_D64BE_g hdf5-lib hid_t)
(define-c H5T_UNIX_D64LE_g hdf5-lib hid_t)
(define H5T_UNIX_D32BE H5T_UNIX_D32BE_g)
(define H5T_UNIX_D32LE H5T_UNIX_D32LE_g)
(define H5T_UNIX_D64BE H5T_UNIX_D64BE_g)
(define H5T_UNIX_D64LE H5T_UNIX_D64LE_g)


#|
 * Types particular to the C language.  String types use `bytes' instead
 * of `bits' as their size.
|#
(define-c H5T_C_S1_g hdf5-lib hid_t)
(define H5T_C_S1 H5T_C_S1_g)

#|
 * Types particular to Fortran.
|#
(define-c H5T_FORTRAN_S1_g hdf5-lib hid_t)
(define H5T_FORTRAN_S1 H5T_FORTRAN_S1_g)


#|
 * These types are for Intel CPU's.  They are little endian with IEEE
 * floating point.
 |#
(define H5T_INTEL_I8		H5T_STD_I8LE)
(define H5T_INTEL_I16		H5T_STD_I16LE)
(define H5T_INTEL_I32		H5T_STD_I32LE)
(define H5T_INTEL_I64		H5T_STD_I64LE)
(define H5T_INTEL_U8		H5T_STD_U8LE)
(define H5T_INTEL_U16		H5T_STD_U16LE)
(define H5T_INTEL_U32		H5T_STD_U32LE)
(define H5T_INTEL_U64		H5T_STD_U64LE)
(define H5T_INTEL_B8		H5T_STD_B8LE)
(define H5T_INTEL_B16		H5T_STD_B16LE)
(define H5T_INTEL_B32		H5T_STD_B32LE)
(define H5T_INTEL_B64		H5T_STD_B64LE)
(define H5T_INTEL_F32		H5T_IEEE_F32LE)
(define H5T_INTEL_F64		H5T_IEEE_F64LE)

#|
 * These types are for DEC Alpha CPU's.  They are little endian with IEEE
 * floating point.
 |#
(define H5T_ALPHA_I8		H5T_STD_I8LE)
(define H5T_ALPHA_I16		H5T_STD_I16LE)
(define H5T_ALPHA_I32		H5T_STD_I32LE)
(define H5T_ALPHA_I64		H5T_STD_I64LE)
(define H5T_ALPHA_U8		H5T_STD_U8LE)
(define H5T_ALPHA_U16		H5T_STD_U16LE)
(define H5T_ALPHA_U32		H5T_STD_U32LE)
(define H5T_ALPHA_U64		H5T_STD_U64LE)
(define H5T_ALPHA_B8		H5T_STD_B8LE)
(define H5T_ALPHA_B16		H5T_STD_B16LE)
(define H5T_ALPHA_B32		H5T_STD_B32LE)
(define H5T_ALPHA_B64		H5T_STD_B64LE)
(define H5T_ALPHA_F32		H5T_IEEE_F32LE)
(define H5T_ALPHA_F64		H5T_IEEE_F64LE)

#|
 * These types are for MIPS cpu's commonly used in SGI systems. They are big
 * endian with IEEE floating point.
 |#
(define H5T_MIPS_I8		H5T_STD_I8BE)
(define H5T_MIPS_I16		H5T_STD_I16BE)
(define H5T_MIPS_I32		H5T_STD_I32BE)
(define H5T_MIPS_I64		H5T_STD_I64BE)
(define H5T_MIPS_U8		H5T_STD_U8BE)
(define H5T_MIPS_U16		H5T_STD_U16BE)
(define H5T_MIPS_U32		H5T_STD_U32BE)
(define H5T_MIPS_U64		H5T_STD_U64BE)
(define H5T_MIPS_B8		H5T_STD_B8BE)
(define H5T_MIPS_B16		H5T_STD_B16BE)
(define H5T_MIPS_B32		H5T_STD_B32BE)
(define H5T_MIPS_B64		H5T_STD_B64BE)
(define H5T_MIPS_F32		H5T_IEEE_F32BE)
(define H5T_MIPS_F64		H5T_IEEE_F64BE)

#|
 * The VAX floating point types (i.e. in VAX byte order)
|#
(define-c H5T_VAX_F32_g hdf5-lib hid_t)
(define-c H5T_VAX_F64_g hdf5-lib hid_t)
(define H5T_VAX_F32 H5T_VAX_F32_g)
(define H5T_VAX_F64 H5T_VAX_F64_g)


#|
 * The predefined native types. These are the types detected by H5detect and
 * they violate the naming scheme a little.  Instead of a class name,
 * precision and byte order as the last component, they have a C-like type
 * name.  If the type begins with `U' then it is the unsigned version of the
 * integer type; other integer types are signed.  The type LLONG corresponds
 * to C's `long long' and LDOUBLE is `long double' (these types might be the
 * same as `LONG' and `DOUBLE' respectively).
 *|#
;;#define H5T_NATIVE_CHAR		(CHAR_MIN?H5T_NATIVE_SCHAR:H5T_NATIVE_UCHAR)

(define-c H5T_NATIVE_SCHAR_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UCHAR_g hdf5-lib hid_t)
(define-c H5T_NATIVE_SHORT_g hdf5-lib hid_t)
(define-c H5T_NATIVE_USHORT_g hdf5-lib hid_t)
(define-c H5T_NATIVE_INT_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UINT_g hdf5-lib hid_t)
(define-c H5T_NATIVE_LONG_g hdf5-lib hid_t)
(define-c H5T_NATIVE_ULONG_g hdf5-lib hid_t)
(define-c H5T_NATIVE_LLONG_g hdf5-lib hid_t)
(define-c H5T_NATIVE_ULLONG_g hdf5-lib hid_t)
(define-c H5T_NATIVE_FLOAT_g hdf5-lib hid_t)
(define-c H5T_NATIVE_DOUBLE_g hdf5-lib hid_t)

(define-c H5T_NATIVE_B8_g hdf5-lib hid_t)
(define-c H5T_NATIVE_B16_g hdf5-lib hid_t)
(define-c H5T_NATIVE_B32_g hdf5-lib hid_t)
(define-c H5T_NATIVE_B64_g hdf5-lib hid_t)
(define-c H5T_NATIVE_OPAQUE_g hdf5-lib hid_t)
(define-c H5T_NATIVE_HADDR_g hdf5-lib hid_t)
(define-c H5T_NATIVE_HSIZE_g hdf5-lib hid_t)
(define-c H5T_NATIVE_HSSIZE_g hdf5-lib hid_t)
(define-c H5T_NATIVE_HERR_g hdf5-lib hid_t)
(define-c H5T_NATIVE_HBOOL_g hdf5-lib hid_t)

(define-c H5T_NATIVE_LDOUBLE_g hdf5-lib hid_t)

(define H5T_NATIVE_SCHAR H5T_NATIVE_SCHAR_g)
(define H5T_NATIVE_UCHAR H5T_NATIVE_UCHAR_g)
(define H5T_NATIVE_SHORT H5T_NATIVE_SHORT_g)
(define H5T_NATIVE_USHORT H5T_NATIVE_USHORT_g)
(define H5T_NATIVE_INT H5T_NATIVE_INT_g)
(define H5T_NATIVE_UINT H5T_NATIVE_UINT_g)
(define H5T_NATIVE_LONG H5T_NATIVE_LONG_g)
(define H5T_NATIVE_ULONG H5T_NATIVE_ULONG_g)
(define H5T_NATIVE_LLONG H5T_NATIVE_LLONG_g)
(define H5T_NATIVE_ULLONG H5T_NATIVE_ULLONG_g)
(define H5T_NATIVE_FLOAT H5T_NATIVE_FLOAT_g)
(define H5T_NATIVE_DOUBLE H5T_NATIVE_DOUBLE_g)

(define H5T_NATIVE_LDOUBLE H5T_NATIVE_LDOUBLE_g)

(define H5T_NATIVE_B8 H5T_NATIVE_B8_g)
(define H5T_NATIVE_B16 H5T_NATIVE_B16_g)
(define H5T_NATIVE_B32 H5T_NATIVE_B32_g)
(define H5T_NATIVE_B64 H5T_NATIVE_B64_g)
(define H5T_NATIVE_OPAQUE H5T_NATIVE_OPAQUE_g)
(define H5T_NATIVE_HADDR H5T_NATIVE_HADDR_g)
(define H5T_NATIVE_HSIZE H5T_NATIVE_HSIZE_g)
(define H5T_NATIVE_HSSIZE H5T_NATIVE_HSSIZE_g)
(define H5T_NATIVE_HERR H5T_NATIVE_HERR_g)
(define H5T_NATIVE_HBOOL H5T_NATIVE_HBOOL_g)




;; C9x integer types
(define-c H5T_NATIVE_INT8_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UINT8_g hdf5-lib hid_t)
(define-c H5T_NATIVE_INT_LEAST8_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UINT_LEAST8_g hdf5-lib hid_t)
(define-c H5T_NATIVE_INT_FAST8_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UINT_FAST8_g hdf5-lib hid_t)
(define H5T_NATIVE_INT8 H5T_NATIVE_INT8_g)
(define H5T_NATIVE_UINT8 H5T_NATIVE_UINT8_g)
(define H5T_NATIVE_INT_LEAST8 H5T_NATIVE_INT_LEAST8_g)
(define H5T_NATIVE_UINT_LEAST8 H5T_NATIVE_UINT_LEAST8_g)
(define H5T_NATIVE_INT_FAST8 H5T_NATIVE_INT_FAST8_g)
(define H5T_NATIVE_UINT_FAST8 H5T_NATIVE_UINT_FAST8_g)

(define-c H5T_NATIVE_INT16_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UINT16_g hdf5-lib hid_t)
(define-c H5T_NATIVE_INT_LEAST16_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UINT_LEAST16_g hdf5-lib hid_t)
(define-c H5T_NATIVE_INT_FAST16_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UINT_FAST16_g hdf5-lib hid_t)
(define H5T_NATIVE_INT16 H5T_NATIVE_INT16_g)
(define H5T_NATIVE_UINT16 H5T_NATIVE_UINT16_g)
(define H5T_NATIVE_INT_LEAST16 H5T_NATIVE_INT_LEAST16_g)
(define H5T_NATIVE_UINT_LEAST16 H5T_NATIVE_UINT_LEAST16_g)
(define H5T_NATIVE_INT_FAST16 H5T_NATIVE_INT_FAST16_g)
(define H5T_NATIVE_UINT_FAST16 H5T_NATIVE_UINT_FAST16_g)

(define-c H5T_NATIVE_INT32_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UINT32_g hdf5-lib hid_t)
(define-c H5T_NATIVE_INT_LEAST32_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UINT_LEAST32_g hdf5-lib hid_t)
(define-c H5T_NATIVE_INT_FAST32_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UINT_FAST32_g hdf5-lib hid_t)
(define H5T_NATIVE_INT32 H5T_NATIVE_INT32_g)
(define H5T_NATIVE_UINT32 H5T_NATIVE_UINT32_g)
(define H5T_NATIVE_INT_LEAST32 H5T_NATIVE_INT_LEAST32_g)
(define H5T_NATIVE_UINT_LEAST32 H5T_NATIVE_UINT_LEAST32_g)
(define H5T_NATIVE_INT_FAST32 H5T_NATIVE_INT_FAST32_g)
(define H5T_NATIVE_UINT_FAST32 H5T_NATIVE_UINT_FAST32_g)

(define-c H5T_NATIVE_INT64_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UINT64_g hdf5-lib hid_t)
(define-c H5T_NATIVE_INT_LEAST64_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UINT_LEAST64_g hdf5-lib hid_t)
(define-c H5T_NATIVE_INT_FAST64_g hdf5-lib hid_t)
(define-c H5T_NATIVE_UINT_FAST64_g hdf5-lib hid_t)
(define H5T_NATIVE_INT64 H5T_NATIVE_INT64_g)
(define H5T_NATIVE_UINT64 H5T_NATIVE_UINT64_g)
(define H5T_NATIVE_INT_LEAST64 H5T_NATIVE_INT_LEAST64_g)
(define H5T_NATIVE_UINT_LEAST64 H5T_NATIVE_UINT_LEAST64_g)
(define H5T_NATIVE_INT_FAST64 H5T_NATIVE_INT_FAST64_g)
(define H5T_NATIVE_UINT_FAST64 H5T_NATIVE_UINT_FAST64_g)



;; Operations defined on all datatypes
(define-hdf5 H5Tcreate
  (_fun (type : H5T_class_t)
        (size : _size)
        -> hid_t))

(define-hdf5 H5Tcopy
  (_fun (type_id : hid_t)
        -> hid_t))

(define-hdf5 H5Tclose
  (_fun (type_id : hid_t)
        -> herr_t))

(define-hdf5 H5Tequal
  (_fun (type1_id : hid_t)
        (type2_id : hid_t)
        -> htri_t))

(define-hdf5 H5Tlock
  (_fun (type_id : hid_t)
        -> herr_t))

(define-hdf5 H5Tcommit2
  (_fun (loc_id : hid_t)
        (name : _string)
        (type_id : hid_t)
        (lcpl_id : hid_t)
        (tcpl_id : hid_t)
        (tapl_id : hid_t)
        -> herr_t))

(define H5Tcommit H5Tcommit2)


(define-hdf5 H5Topen2
  (_fun (loc_id : hid_t)
        (name : _string)
        (tapl_id : hid_t)
        -> hid_t))

(define H5Topen H5Topen2)

(define-hdf5 H5Tcommit_anon
  (_fun (loc_id : hid_t)
        (type_id : hid_t)
        (tcpl_id : hid_t)
        (tapl_id : hid_t )
        -> herr_t))

(define-hdf5 H5Tget_create_plist
  (_fun (type_id : hid_t)
        -> hid_t))

(define-hdf5 H5Tcommitted
  (_fun (type_id : hid_t)
        -> htri_t))

(define-hdf5 H5Tencode
  (_fun (obj_id : hid_t)
        (buf : _pointer)
        (nalloc : _pointer)
        -> herr_t))


(define-hdf5 H5Tdecode
  (_fun (buf : _pointer)
        -> hid_t))


;; Operations defined on compound datatypes
(define-hdf5 H5Tinsert
  (_fun (parent_id : hid_t)
        (name : _string)
        (offset : _size)
        (member_id : hid_t)
        -> herr_t))

(define-hdf5 H5Tpack
  (_fun (type_id : hid_t)
        -> herr_t))

;; Operations defined on enumeration datatypes
(define-hdf5 H5Tenum_create
  (_fun (base_id : hid_t)
        -> hid_t))

(define-hdf5 H5Tenum_insert
  (_fun (type : hid_t)
        (name : _string)
        (value : _pointer)
        -> herr_t))

(define-hdf5 H5Tenum_nameof
  (_fun (type value size) ::
        (type : hid_t)
        (value : _pointer)
        (name : _pointer = (malloc _string size))
        (size : _size)
        -> (status : herr_t)
        -> (if (< status 0) 
            (error 'H5Tenum_nameof "Unable to get the name of enum value: ~a~n" value)
            (cast name _pointer _string))))

(define-hdf5 H5Tenum_valueof
  (_fun (type : hid_t)
        (name : _string)
        (value : _pointer) ;; TODO: out
        -> herr_t))

;; Operations defined on variable-length datatypes
(define-hdf5 H5Tvlen_create
  (_fun (base_id : hid_t)
        -> hid_t))

;; Operations defined on array datatypes
(define-hdf5 H5Tarray_create2
  (_fun (base_id : hid_t)
        (ndims : _uint)
        (dim : hsize_t) ;; TODO: [ndims]
        -> hid_t))

(define-hdf5 H5Tget_array_ndims
  (_fun (type_id : hid_t)
        -> _int))

(define-hdf5 H5Tget_array_dims2
  (_fun (type_id : hid_t)
        (dims : _pointer)
        -> _int))

;; Operations defined on opaque datatypes
(define-hdf5 H5Tset_tag
  (_fun (type : hid_t)
        (tag : _string)
        -> herr_t))

(define-hdf5 H5Tget_tag
  (_fun (type : hid_t)
        -> _pointer)) ;; TODO *char->string?

;; Querying property values
(define-hdf5 H5Tget_super
  (_fun (type : hid_t)
        -> hid_t))

(define-hdf5 H5Tget_class
  (_fun (type_id : hid_t)
        -> H5T_class_t))

(define-hdf5 H5Tdetect_class
  (_fun (type_id : hid_t)
        (cls : H5T_class_t)
        -> htri_t))

(define-hdf5 H5Tget_size
  (_fun (type_id : hid_t)
        -> _size))

(define-hdf5 H5Tget_order
  (_fun (type_id : hid_t)
        -> H5T_order_t))

(define-hdf5 H5Tget_precision
  (_fun (type_id : hid_t)
        -> _size))

(define-hdf5 H5Tget_offset
  (_fun (type_id : hid_t)
        -> _int))

(define-hdf5 H5Tget_pad
  (_fun (type_id : hid_t)
        (lsb : _pointer) ;; TODO: H5T_pad_t *lsb/*out*/
        (msb : _pointer) ;; TODO: H5T_pad_t *msb/*out*/
        -> herr_t))

(define-hdf5 H5Tget_sign
  (_fun (type_id : hid_t)
        -> H5T_sign_t))

(define-hdf5 H5Tget_fields
  (_fun (type_id : hid_t)
        (spos : _pointer) ;; TODO: size_t *spos/*out*/,
        (epos : _pointer) ;; TODO: size_t *epos/*out*/
        (esize : _pointer);; TODO: size_t *esize/*out*/
        (mpos : _pointer) ;; TODO: size_t *mpos/*out*/
        (msize : _pointer) ;; TODO: size_t *msize/*out*/
        -> herr_t))

(define-hdf5 H5Tget_ebias
  (_fun (type_id : hid_t)
        -> _size))

(define-hdf5 H5Tget_norm
  (_fun (type_id : hid_t)
        -> H5T_norm_t))

(define-hdf5 H5Tget_inpad
  (_fun (type_id : hid_t)
        -> H5T_pad_t))

(define-hdf5 H5Tget_strpad
  (_fun (type_id : hid_t)
        -> H5T_str_t))

(define-hdf5 H5Tget_nmembers
  (_fun (type_id : hid_t)
        -> _int))


(define-hdf5 H5Tget_member_name
  (_fun (type_id : hid_t)
        (membno : _uint)
        -> (name : _bytes)
        -> (~a name))) ;; TODO char->string?

(define-hdf5 H5Tget_member_index
  (_fun (type_id : hid_t)
        (name : _string)
        -> _int))

(define-hdf5 H5Tget_member_offset
  (_fun (type_id : hid_t)
        (membno : _uint)
        -> _size))

(define-hdf5 H5Tget_member_class
  (_fun (type_id : hid_t)
        (membno : _uint)
        -> H5T_class_t))

(define-hdf5 H5Tget_member_type
  (_fun (type_id : hid_t)
        (membno : _uint)
        -> hid_t))

(define-hdf5 H5Tget_member_value
  (_fun (type_id : hid_t)
        (membno : _uint)
        (value : _pointer) ;; TODO: out
        -> herr_t))

(define-hdf5 H5Tget_cset
  (_fun (type_id : hid_t)
        -> H5T_cset_t))

(define-hdf5 H5Tis_variable_str
  (_fun (type_id : hid_t)
        -> htri_t))

(define-hdf5 H5Tget_native_type
  (_fun (type_id : hid_t)
        (direction : H5T_direction_t)
        -> hid_t))


;; Setting property values
(define-hdf5 H5Tset_size
  (_fun (type_id : hid_t)
        (size : _size)
        -> herr_t))

(define-hdf5 H5Tset_order
  (_fun (type_id : hid_t)
        (order : H5T_order_t)
        -> herr_t))

(define-hdf5 H5Tset_precision
  (_fun (type_id : hid_t)
        (prec : _size)
        -> herr_t))

(define-hdf5 H5Tset_offset
  (_fun (type_id : hid_t)
        (offset : _size)
        -> herr_t))

(define-hdf5 H5Tset_pad
  (_fun (type_id : hid_t)
        (lsb : H5T_pad_t)
        (msb : H5T_pad_t)
        -> herr_t))

(define-hdf5 H5Tset_sign
  (_fun (type_id : hid_t)
        (sign : H5T_sign_t)
        -> herr_t))

(define-hdf5 H5Tset_fields
  (_fun (type_id : hid_t)
        (spos : _size)
        (epos : _size)
        (esize : _size)
        (mpos : _size)
        (msize : _size)
        -> herr_t))

(define-hdf5 H5Tset_ebias
  (_fun (type_id : hid_t)
        (ebias : _size)
        -> herr_t))

(define-hdf5 H5Tset_norm
  (_fun (type_id : hid_t)
        (norm : H5T_norm_t)
        -> herr_t))

(define-hdf5 H5Tset_inpad
  (_fun (type_id : hid_t)
        (pad : H5T_pad_t)
        -> herr_t))

(define-hdf5 H5Tset_cset
  (_fun (type_id : hid_t)
        (cset : H5T_cset_t)
        -> herr_t))

(define-hdf5 H5Tset_strpad
  (_fun (type_id : hid_t)
        (strpad : H5T_str_t)
        -> herr_t))

;; Type conversion database
(define-hdf5 H5Tregister
  (_fun (pers : H5T_pers_t)
        (name : _string)
        (src_id : hid_t)
        (dst_id : hid_t)
        (func : _pointer)
        -> herr_t))

(define-hdf5 H5Tunregister
  (_fun (pers : H5T_pers_t)
        (name : _string)
        (src_id : hid_t)
        (dst_id : hid_t)
        (func : _pointer)
        -> herr_t))

(define-hdf5 H5Tfind
  (_fun (src_id : hid_t)
        (dst_id : hid_t)
        (pcdata : _pointer)
        -> _pointer))

(define-hdf5 H5Tcompiler_conv
  (_fun (src_id : hid_t)
        (dst_id : hid_t)
        -> htri_t))

(define-hdf5 H5Tconvert
  (_fun (src_id : hid_t)
        (dst_id : hid_t)
        (nelmts : _size)
        (buf : _pointer)
        (background : _pointer)
        (plist_id : hid_t)
        -> herr_t))


#| Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 |#
;; Function prototypes
(define-hdf5 H5Tcommit1
  (_fun (loc_id : hid_t)
        (name : _string)
        (type_id : hid_t)
        -> herr_t))

(define-hdf5 H5Topen1
  (_fun (loc_id : hid_t)
        (name : _string)
        -> hid_t))

(define-hdf5 H5Tarray_create1
  (_fun (base_id : hid_t)
        (ndims : _int)
        (dim : _pointer) ;; TODO: [/* ndims */]
        (perm : _pointer) ;; TODO: [/* ndims */]
        -> hid_t))

(define-hdf5 H5Tget_array_dims1
  (_fun (type_id : hid_t)
        (dims : _pointer)
        (perm : _pointer)
        -> _int))
