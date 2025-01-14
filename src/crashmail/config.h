#ifndef CONFIG_H
#define CONFIG_H

#define READCONFIG_NOT_FOUND  1
#define READCONFIG_INVALID    2
#define READCONFIG_NO_MEM     3

#define CFG_CHECKSEENBY            (1L<<0)
#define CFG_CHECKPKTDEST           (1L<<1)
#define CFG_STRIPRE                (1L<<2)
#define CFG_FORCEINTL              (1L<<3)
#define CFG_NOROUTE                (1L<<4)
#define CFG_PATH3D                 (1L<<5)
#define CFG_IMPORTSEENBY           (1L<<6)
#define CFG_IMPORTEMPTYNETMAIL     (1L<<7)
#define CFG_BOUNCEPOINTS           (1L<<8)
#define CFG_ANSWERRECEIPT          (1L<<9)
#define CFG_ANSWERAUDIT            (1L<<10)
#define CFG_NODIRECTATTACH         (1L<<11)
#define CFG_IMPORTAREAFIX          (1L<<12)
#define CFG_AREAFIXREMOVE          (1L<<13)
#define CFG_WEEKDAYNAMING          (1L<<14)
#define CFG_ADDTID                 (1L<<16)
#define CFG_ALLOWRESCAN            (1L<<18)
#define CFG_FORWARDPASSTHRU        (1L<<19)
#define CFG_BOUNCEHEADERONLY       (1L<<21)
#define CFG_REMOVEWHENFEED         (1L<<22)
#define CFG_INCLUDEFORWARD         (1L<<23)
#define CFG_NOMAXOUTBOUNDZONE      (1L<<24)
#define CFG_ALLOWKILLSENT          (1L<<25)

#ifdef MSGBASE_MSG
#define CFG_MSG_HIGHWATER         (1L<<1)
#define CFG_MSG_WRITEBACK         (1L<<2)
#endif

#ifdef MSGBASE_UMS
#define CFG_UMS_MAUSGATE               (1L<<0)
#define CFG_UMS_KEEPORIGIN             (1L<<1)
#define CFG_UMS_CHANGEMSGID            (1L<<2)
#define CFG_UMS_IGNOREORIGINDOMAIN     (1L<<3)
#define CFG_UMS_EMPTYTOALL             (1L<<4)
#endif

#ifdef MSGBASE_JAM
#define CFG_JAM_HIGHWATER     (1L<<0)
#define CFG_JAM_LINK          (1L<<0)
#define CFG_JAM_QUICKLINK     (1L<<1)
#endif

#ifdef PLATFORM_AMIGA
#define CFG_AMIGA_UNATTENDED        (1L<<0)
#endif

#define AREA_NETMAIL        1
#define AREA_UNCONFIRMED    2
#define AREA_BAD            4
#define AREA_MANDATORY      8
#define AREA_DEFREADONLY   16
#define AREA_IGNOREDUPES   32
#define AREA_IGNORESEENBY  64
#define AREA_DEFAULT      128

struct Area
{
   struct Area *Next;
   bool changed;
   struct Aka  *Aka;
   uchar Flags;
   uchar Tagname[80];
   uchar Description[80];
   struct Messagebase *Messagebase;
   uchar Path[80];
   uchar Group;

   struct jbList TossNodes;
   struct jbList BannedNodes;

   /* Maint */

   ulong KeepNum,KeepDays;

   /* Stats */

   ulong Texts;
   ulong NewTexts;
   ulong Dupes;
   ulong NewDupes;
   ushort Last8Days[8];
   time_t FirstTime;
   time_t LastTime;
};

struct Aka
{
   struct Aka *Next;
   struct Node4D Node;
   uchar Domain[20];
   struct jbList AddList;
   struct jbList RemList;
};

#define TOSSNODE_READONLY  1
#define TOSSNODE_WRITEONLY 2
#define TOSSNODE_FEED      4

struct TossNode
{
   struct TossNode *Next;
   struct ConfigNode *ConfigNode;
   ushort Flags;
};

struct ImportNode
{
   struct ImportNode *Next;
   struct Node4D Node;
};

struct BannedNode
{
   struct BannedNode *Next;
   struct ConfigNode *ConfigNode;
};

#define NODE_AUTOADD       1
#define NODE_PASSIVE       2
#define NODE_TINYSEENBY    4
#define NODE_NOSEENBY      8
#define NODE_FORWARDREQ   16
#define NODE_NOTIFY       32
#define NODE_PACKNETMAIL  64
#define NODE_SENDAREAFIX 128
#define NODE_SENDTEXT    256

struct RemoteAFCommand
{
   struct RemoteAFCommand *Next;
   uchar Command[80];
};

struct ConfigNode
{
   struct ConfigNode *Next;
   bool changed;
   struct Node4D Node;
   uchar PacketPW[9];
   struct Packer *Packer;
   bool IsInSeenBy;
   uchar AreafixPW[40];
   uchar Groups[30];
   uchar ReadOnlyGroups[30];
   uchar AddGroups[30];
   uchar DefaultGroup;
   ulong Flags;
   uchar SysopName[36];
	uchar EchomailPri;
	
   uchar RemoteAFName[36];
   uchar RemoteAFPw[72];

   struct jbList RemoteAFList;

	uchar LastArcName[12];
	
   /* Stats */

   ulong GotNetmails;
   ulong GotNetmailBytes;
   ulong SentNetmails;
   ulong SentNetmailBytes;

   ulong GotEchomails;
   ulong GotEchomailBytes;
   ulong SentEchomails;
   ulong SentEchomailBytes;

   ulong Dupes;

   time_t FirstTime;
};

struct Packer
{
   struct Packer *Next;
   uchar Name[10];
   uchar Packer[80];
   uchar Unpacker[80];
   uchar Recog[80];
};

struct AddNode
{
   struct AddNode *Next;
   struct Node4D Node;
};

struct RemNode
{
   struct RemNode *Next;
   struct Node2DPat NodePat;
};

struct Route
{
   struct Route      *Next;
   struct Node4DPat Pattern;
   struct Node4DPat DestPat;
   struct Aka        *Aka;
};

struct PatternNode
{
   struct PatternNode *Next;
   struct Node4DPat Pattern;
};

#define PKTS_ECHOMAIL  0
#define PKTS_HOLD      1
#define PKTS_NORMAL    2
#define PKTS_DIRECT    3
#define PKTS_CRASH     4

struct Change
{
   struct Change *Next;
   struct Node4DPat Pattern;
   bool ChangeNormal;
   bool ChangeCrash;
   bool ChangeDirect;
   bool ChangeHold;
   uchar DestPri;
};

struct Remap
{
   struct Remap *Next;
   uchar Pattern[200];
   uchar NewTo[36];
   struct Node4DPat DestPat;
};

struct RemapNode
{
   struct RemapNode *Next;
   struct Node4DPat NodePat;
   struct Node4DPat DestPat;
};

struct Robot
{
   struct Robot *Next;
   uchar Pattern[200];
   uchar Command[200];
};

struct AreaFixName
{
   struct AreaFixName *Next;
   uchar Name[36];
};

#define AREALIST_DESC     1
#define AREALIST_FORWARD  2

struct Arealist
{
   struct Arealist *Next;
   struct ConfigNode *Node;
   uchar AreaFile[80];
   ushort Flags;
   uchar Group;
};

#define DUPE_IGNORE 0
#define DUPE_BAD    1
#define DUPE_KILL   2

#define LOOP_IGNORE  0
#define LOOP_LOG     1
#define LOOP_LOGBAD  2

struct Config
{
   bool changed;
   uchar filename[100];
   
   uchar cfg_Sysop[36];
   uchar cfg_Inbound[100];
   uchar cfg_Outbound[100];
   uchar cfg_TempDir[100];
   uchar cfg_PacketCreate[100];
   uchar cfg_PacketDir[100];
   uchar cfg_LogFile[100];
   uchar cfg_StatsFile[100];
   uchar cfg_DupeFile[100];
   ulong cfg_LogLevel;
   ulong cfg_DupeSize;
   ulong cfg_MaxPktSize;
   ulong cfg_MaxBundleSize;
   uchar cfg_AreaFixHelp[100];
   uchar cfg_Nodelist[100];
   struct Nodelist *cfg_NodelistType;
   ulong cfg_AreaFixMaxLines;
   uchar cfg_GroupNames[30][80];
   ulong cfg_Flags;
   ushort cfg_DupeMode;
   ushort cfg_LoopMode;
   ulong cfg_DefaultZone;
   struct jbList AkaList;
   struct jbList AreaList;
   struct jbList CNodeList;
   struct jbList PackerList;
   struct jbList RouteList;
   struct jbList FileAttachList;
   struct jbList BounceList;
   struct jbList ChangeList;
   struct jbList RemapList;
   struct jbList RemapNodeList;
   struct jbList RobotList;
   struct jbList AreaFixList;
   struct jbList ArealistList;

#ifdef PLATFORM_AMIGA
   ulong cfg_amiga_LogBufferLines;
   ulong cfg_amiga_LogBufferSecs;
   ulong cfg_amiga_Flags;
#endif

#ifdef MSGBASE_UMS
   uchar cfg_ums_RFCGatewayName[40];
   struct Node4D cfg_ums_RFCGatewayNode;
   uchar cfg_ums_LoginName[80];
   uchar cfg_ums_LoginPassword[80];
   uchar cfg_ums_LoginServer[80];
   uchar cfg_ums_GatewayName[36];
   ulong cfg_ums_Flags;
#endif

#ifdef MSGBASE_MSG
   ulong cfg_msg_Flags;
#endif

#ifdef MSGBASE_JAM
   ulong cfg_jam_MaxOpen;
   ulong cfg_jam_Flags;
#endif
};

bool ReadConfig(uchar *filename,struct Config *cfg,short *seconderr,ulong *cfgline,uchar *cfgerr);
bool UpdateConfig(struct Config *cfg,uchar *cfgerr);
void InitConfig(struct Config *cfg);
void FreeConfig(struct Config *cfg);

bool CheckConfig(struct Config *cfg,uchar *cfgerr);
/* Should not be called in prefs */

#endif


