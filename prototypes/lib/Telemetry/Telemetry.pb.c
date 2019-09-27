/* Automatically generated nanopb constant definitions */
/* Generated by nanopb-0.3.9.3 at Fri Sep 27 15:18:45 2019. */

#include "Telemetry.pb.h"

/* @@protoc_insertion_point(includes) */
#if PB_PROTO_HEADER_VERSION != 30
#error Regenerate this file with the current version of nanopb generator.
#endif



const pb_field_t Telemetry_fields[4] = {
    PB_FIELD(  1, UENUM   , SINGULAR, STATIC  , FIRST, Telemetry, type, type, 0),
    PB_FIELD(  2, MESSAGE , SINGULAR, STATIC  , OTHER, Telemetry, hello, type, &MsgHello_fields),
    PB_FIELD(  3, MESSAGE , SINGULAR, STATIC  , OTHER, Telemetry, action, hello, &MsgAction_fields),
    PB_LAST_FIELD
};

const pb_field_t MsgHello_fields[3] = {
    PB_FIELD(  1, STRING  , SINGULAR, STATIC  , FIRST, MsgHello, ip, ip, 0),
    PB_FIELD(  2, UINT32  , SINGULAR, STATIC  , OTHER, MsgHello, version, ip, 0),
    PB_LAST_FIELD
};

const pb_field_t MsgAction_fields[3] = {
    PB_FIELD(  1, UENUM   , SINGULAR, STATIC  , FIRST, MsgAction, type, type, 0),
    PB_FIELD(  2, UINT32  , SINGULAR, STATIC  , OTHER, MsgAction, pin, type, 0),
    PB_LAST_FIELD
};




/* Check that field information fits in pb_field_t */
#if !defined(PB_FIELD_32BIT)
/* If you get an error here, it means that you need to define PB_FIELD_32BIT
 * compile-time option. You can do that in pb.h or on compiler command line.
 * 
 * The reason you need to do this is that some of your messages contain tag
 * numbers or field sizes that are larger than what can fit in 8 or 16 bit
 * field descriptors.
 */
PB_STATIC_ASSERT((pb_membersize(Telemetry, hello) < 65536 && pb_membersize(Telemetry, action) < 65536), YOU_MUST_DEFINE_PB_FIELD_32BIT_FOR_MESSAGES_Telemetry_MsgHello_MsgAction)
#endif

#if !defined(PB_FIELD_16BIT) && !defined(PB_FIELD_32BIT)
/* If you get an error here, it means that you need to define PB_FIELD_16BIT
 * compile-time option. You can do that in pb.h or on compiler command line.
 * 
 * The reason you need to do this is that some of your messages contain tag
 * numbers or field sizes that are larger than what can fit in the default
 * 8 bit descriptors.
 */
PB_STATIC_ASSERT((pb_membersize(Telemetry, hello) < 256 && pb_membersize(Telemetry, action) < 256), YOU_MUST_DEFINE_PB_FIELD_16BIT_FOR_MESSAGES_Telemetry_MsgHello_MsgAction)
#endif


/* @@protoc_insertion_point(eof) */
