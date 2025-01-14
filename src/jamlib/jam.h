/***********************************************************************
**
**  JAM Definitions
**
***********************************************************************/

#ifndef __JAM_H__
#define __JAM_H__

#include <stdio.h>
#include <ctype.h>

#ifndef __UCHAR_DEFINED__
#define __UCHAR_DEFINED__

typedef unsigned char  uchar;    /* must be  8 bits wide */
typedef unsigned short ushort;   /* must be 16 bits wide */
typedef unsigned long  ulong;    /* must be 32 bits wide */

#endif

/*
**  Error codes
*/
#define JAM_BAD_PARAM   1  /* one or more parameters are wrong */
#define JAM_IO_ERROR		2  /* i/o error. check JAM_Errno() for details */
#define JAM_LOCK_FAILED 3  /* lock could not be set */
#define JAM_NOT_LOCKED  4  /* the message base was not locked before writing */
#define JAM_NO_MEMORY   5  /* out of memory! */
#define JAM_NO_USER	6  /* user not found */
#define JAM_NO_MESSAGE  7  /* message has been deleted */
/*
**  CRC definitions
*/

#define JAM_NO_CRC	0xffffffff

/*
**  File extensions
*/
#define EXT_HDRFILE     ".jhr"
#define EXT_TXTFILE     ".jdt"
#define EXT_IDXFILE     ".jdx"
#define EXT_LRDFILE     ".jlr"

/*
**  Revision level and header signature
*/
#define CURRENTREVLEV   1
#define HEADERSIGNATURE "JAM\0"

/*
**  Header file information block, stored first in all .JHR files
*/
typedef struct {
    uchar  Signature[4];      /* <J><A><M> followed by <NUL> */
    ulong  DateCreated;       /* Creation date */
    ulong  ModCounter;        /* Last processed counter */
    ulong  ActiveMsgs;        /* Number of active (not deleted) msgs */
    ulong  PasswordCRC;       /* CRC-32 of password to access */
    ulong  BaseMsgNum;        /* Lowest message number in index file */
    uchar  RSRVD[1000];       /* Reserved space */
} s_JamBaseHeader;

/*
**  Message status bits
*/
#define MSG_LOCAL       0x00000001L /* Msg created locally */
#define MSG_INTRANSIT   0x00000002L /* Msg is in-transit */
#define MSG_PRIVATE     0x00000004L /* Private */
#define MSG_READ        0x00000008L /* Read by addressee */
#define MSG_SENT        0x00000010L /* Sent to remote */
#define MSG_KILLSENT    0x00000020L /* Kill when sent */
#define MSG_ARCHIVESENT 0x00000040L /* Archive when sent */
#define MSG_HOLD        0x00000080L /* Hold for pick-up */
#define MSG_CRASH       0x00000100L /* Crash */
#define MSG_IMMEDIATE   0x00000200L /* Send Msg now, ignore restrictions */
#define MSG_DIRECT      0x00000400L /* Send directly to destination */
#define MSG_GATE        0x00000800L /* Send via gateway */
#define MSG_FILEREQUEST 0x00001000L /* File request */
#define MSG_FILEATTACH  0x00002000L /* File(s) attached to Msg */
#define MSG_TRUNCFILE   0x00004000L /* Truncate file(s) when sent */
#define MSG_KILLFILE    0x00008000L /* Delete file(s) when sent */
#define MSG_RECEIPTREQ  0x00010000L /* Return receipt requested */
#define MSG_CONFIRMREQ  0x00020000L /* Confirmation receipt requested */
#define MSG_ORPHAN      0x00040000L /* Unknown destination */
#define MSG_ENCRYPT     0x00080000L /* Msg text is encrypted */
#define MSG_COMPRESS    0x00100000L /* Msg text is compressed */
#define MSG_ESCAPED     0x00200000L /* Msg text is seven bit ASCII */
#define MSG_FPU         0x00400000L /* Force pickup */
#define MSG_TYPELOCAL   0x00800000L /* Msg is for local use only (no export) */
#define MSG_TYPEECHO    0x01000000L /* Msg is for conference distribution */
#define MSG_TYPENET     0x02000000L /* Msg is direct network mail */
#define MSG_NODISP      0x20000000L /* Msg may not be displayed to user */
#define MSG_LOCKED      0x40000000L /* Msg is locked, no editing possible */
#define MSG_DELETED     0x80000000L /* Msg is deleted */

/*
**  Message header
*/
typedef struct {
    uchar  Signature[4];              /* <J><A><M> followed by <NUL> */
    ushort Revision;                  /* CURRENTREVLEV */
    ushort ReservedWord;              /* Reserved */
    ulong  SubfieldLen;               /* Length of Subfields */
    ulong  TimesRead;                 /* Number of times message read */
    ulong  MsgIdCRC;                  /* CRC-32 of MSGID line */
    ulong  ReplyCRC;                  /* CRC-32 of REPLY line */
    ulong  ReplyTo;                   /* This msg is a reply to.. */
    ulong  Reply1st;                  /* First reply to this msg */
    ulong  ReplyNext;                 /* Next msg in reply chain */
    ulong  DateWritten;               /* When msg was written */
    ulong  DateReceived;              /* When msg was received/read */
    ulong  DateProcessed;             /* When msg was processed by packer */
    ulong  MsgNum;                    /* Message number (1-based) */
    ulong  Attribute;                 /* Msg attribute, see "Status bits" */
    ulong  Attribute2;                /* Reserved for future use */
    ulong  TxtOffset;                 /* Offset of text in text file */
    ulong  TxtLen;                    /* Length of message text */
    ulong  PasswordCRC;               /* CRC-32 of password to access msg */
    ulong  Cost;                      /* Cost of message */
} s_JamMsgHeader;

/*
**  Message header Subfield types
*/
#define JAMSFLD_OADDRESS    0
#define JAMSFLD_DADDRESS    1
#define JAMSFLD_SENDERNAME  2
#define JAMSFLD_RECVRNAME   3
#define JAMSFLD_MSGID       4
#define JAMSFLD_REPLYID     5
#define JAMSFLD_SUBJECT     6
#define JAMSFLD_PID         7
#define JAMSFLD_TRACE       8
#define JAMSFLD_ENCLFILE    9
#define JAMSFLD_ENCLFWALIAS 10
#define JAMSFLD_ENCLFREQ    11
#define JAMSFLD_ENCLFILEWC  12
#define JAMSFLD_ENCLINDFILE 13
#define JAMSFLD_EMBINDAT    1000
#define JAMSFLD_FTSKLUDGE   2000
#define JAMSFLD_SEENBY2D    2001
#define JAMSFLD_PATH2D      2002
#define JAMSFLD_FLAGS       2003
#define JAMSFLD_TZUTCINFO   2004
#define JAMSFLD_UNKNOWN     0xffff

/*
**  Message header Subfield
*/
typedef struct {
    ushort LoID;       /* Field ID, 0 - 0xffff */
    ushort HiID;       /* Reserved for future use */
    ulong  DatLen;     /* Length of buffer that follows */
    uchar* Buffer;     /* DatLen bytes of data */
} s_JamSubfield;

typedef struct {
    ushort LoID;       /* Field ID, 0 - 0xffff */
    ushort HiID;       /* Reserved for future use */
    ulong  DatLen;     /* Length of buffer that follows */
} s_JamSaveSubfield;

/*
**  Message index record
*/
typedef struct {
    ulong  UserCRC;    /* CRC-32 of destination username */
    ulong  HdrOffset;  /* Offset of header in .JHR file */
} s_JamIndex;

/*
**  Lastread structure, one per user
*/
typedef struct {
    ulong  UserCRC;     /* CRC-32 of user name (lowercase) */
    ulong  UserID;      /* Unique UserID */
    ulong  LastReadMsg; /* Last read message number */
    ulong  HighReadMsg; /* Highest read message number */
} s_JamLastRead;

/*
**  JAMLIB message base handle
*/
typedef struct {
    FILE* HdrFile_PS;      /* File handle for .JHR file */
    FILE* TxtFile_PS;      /* File handle for .JDT file */
    FILE* IdxFile_PS;      /* File handle for .JDX file */
    FILE* LrdFile_PS;      /* File handle for .JLR file */
    int   Errno_I;	   /* last i/o error */
    int   Locked_I;	   /* is area locked? */
    ulong LastUserPos_I;   /* last position of lastread record */
    ulong LastUserId_I;    /* userid for the last read lastread record */
} s_JamBase;

/*
**  JAMLIB subfield packet
*/
typedef struct {
    s_JamSubfield** Fields;
    ulong	    NumFields;
    ulong	    NumAlloc;
} s_JamSubPacket;


/*
**  JAMLIB function declarations
*/

/* mbase.c */
int JAM_OpenMB		( uchar* 		Basename_PC,
						  s_JamBase** 		NewArea_PPS );
						  
int JAM_CloseMB	( s_JamBase* 		Area_PS );

int JAM_CreateMB	( uchar* 		Basename_PC,
						  ulong 		BaseMsg_I,
						  s_JamBase** 		NewArea_PPS );

int JAM_RemoveMB	( s_JamBase* 		Area_PS,
						  uchar* 		Basename_PC );

int JAM_LockMB		( s_JamBase* 		Area_PS,
						  int			Timeout_I );

int JAM_UnlockMB	( s_JamBase* 		Area_PS );

int JAM_ReadMBHeader	( s_JamBase* 		Area_PS,
							  s_JamBaseHeader* 	Header_PS );
							  
int JAM_WriteMBHeader	( s_JamBase* 		Area_PS,
								  s_JamBaseHeader* 	Header_PS );
								  
int JAM_FindUser	( s_JamBase* 		Area_PS,
						  ulong 		UserCrc_I,
						  ulong 		StartMsg_I,
						  ulong* 		MsgNo_PI );
int JAM_GetMBSize	( s_JamBase* 		Area_PS,
			 			  ulong* 		Messages_PI );

/* message.c */

int JAM_ReadMsgHeader	( s_JamBase* 		Area_PS, 
								  ulong 		MsgNo_I,
								  s_JamMsgHeader*	Header_PS, 
								  s_JamSubPacket** 	SubfieldPack_PPS );
int JAM_ReadMsgText	( s_JamBase* 		Area_PS, 
							  ulong 		Offset_I,
							  ulong 		Length_I,
							  uchar* 		Buffer_PC );
							  
int JAM_AddMessage	( s_JamBase* 		Area_PS,
							  s_JamMsgHeader*	Header_PS, 
							  s_JamSubPacket*	SubPack_PS,
							  uchar*		Text_PC,
							  ulong			TextLen_I );
							  
int JAM_AddEmptyMessage	(  s_JamBase* 		Area_PS );

int JAM_ChangeMsgHeader	( s_JamBase* 		Area_PS,
								  ulong 		MsgNo_I,
								  s_JamMsgHeader* 	Header_PS );

int JAM_ClearMsgHeader	( s_JamMsgHeader* 	Header_PS );

int JAM_Errno		( s_JamBase* 		Area_PS );

/* lastread.c */

int JAM_ReadLastRead	( s_JamBase* 		Area_PS,
							  ulong 		User_I,
							  s_JamLastRead* 	Record_PS );

int JAM_WriteLastRead	( s_JamBase* 		Area_PS,
								  ulong 		User_I,
								  s_JamLastRead* 	Record_PS );

/* subpacket.c */

s_JamSubPacket* JAM_NewSubPacket	( void );
int 		JAM_DelSubPacket	( s_JamSubPacket* SubPack_PS );
s_JamSubfield* 	JAM_GetSubfield		( s_JamSubPacket* SubPack_PS );
int 		JAM_PutSubfield		( s_JamSubPacket* SubPack_PS,
										  s_JamSubfield*  Field_PS );

/* crc32.c */

ulong JAM_Crc32		( uchar* Buffer_PC, ulong Length_I );

#endif
