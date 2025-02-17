#ifndef MEMMESSAGE_H
#define MEMMESSAGE_H

#define PKT_CHUNKLEN  10000
#define PKT_NUM2D        50
#define PKT_NUMPATH      10

struct TextChunk
{
   struct TextChunk *Next;
   ulong Length;
   uchar Data[PKT_CHUNKLEN];
};

struct Nodes2D
{
   struct Nodes2D *Next;
   ushort Nodes;
   ushort Net[PKT_NUM2D];
   ushort Node[PKT_NUM2D];
};

struct Path
{
   struct Path *Next;
   ushort Paths;
   uchar Path[PKT_NUMPATH][100];
};

struct MemMessage
{
   ulong msgnum;

   struct Node4D OrigNode;
   struct Node4D DestNode;
   struct Node4D PktOrig;
   struct Node4D PktDest;

   uchar Area[40];

   uchar To[36];
   uchar From[36];
   uchar Subject[72];
   uchar DateTime[20];

   uchar MSGID[80];
   uchar REPLY[80];

   ushort Attr;
   ushort Cost;

   uchar Type;
   bool Rescanned;
   bool no_security;
   bool isbounce;
   
   struct jbList TextChunks;
   struct jbList SeenBy;
   struct jbList Path;
};

bool mmAddNodes2DList(struct jbList *list,ushort net,ushort node);
void mmRemNodes2DList(struct jbList *list,ushort net,ushort node);
void mmRemNodes2DListPat(struct jbList *list,struct Node2DPat *pat);
bool mmAddPath(uchar *str,struct jbList *list);
bool mmAddBuf(struct jbList *chunklist,uchar *buf,ulong len);
bool mmAddLine(struct MemMessage *mm,uchar *buf);
struct MemMessage *mmAlloc(void);
void mmClear(struct MemMessage *mm);
void mmFree(struct MemMessage *mm);
bool mmSortNodes2D(struct jbList *list);
uchar *mmMakeSeenByBuf(struct jbList *list);

#endif

