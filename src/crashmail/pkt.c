#include "crashmail.h"

/*************************************************************************/
/*                                                                       */
/*                                Read Pkt                               */
/*                                                                       */
/*************************************************************************/

#define PKT_MINREADLEN 200

bool messageend;
bool shortread,longread;

ushort getuword(uchar *buf,ulong offset)
{
   return (ushort)(buf[offset]+256*buf[offset+1]);
}

void putuword(uchar *buf,ulong offset,ushort num)
{
   buf[offset]=num%256;
   buf[offset+1]=num/256;
}

ulong ReadNull(uchar *buf, ulong maxlen, osFile fh)
{
   /* Reads from fh until buffer full or NULL */

   short ch,c=0;

   if(shortread) return(0);

   ch=osGetChar(fh);

   while(ch!=-1 && ch!=0 && c!=maxlen-1)
   {
      buf[c++]=ch;
      ch=osGetChar(fh);
   }
   buf[c]=0;

   if(ch==-1)
      shortread=TRUE;

   if(ch!=0 && c==maxlen-1)
      longread=TRUE;

   return(c);
}

ulong ReadCR(uchar *buf, ulong maxlen, osFile fh)
{
   /* Reads from fh until buffer full or CR */

   short ch,c=0;

   ch=osGetChar(fh);

   while(ch!=-1 && ch!=0 && ch!=10 && ch !=13 && c!=maxlen-2)
   {
      buf[c++]=ch;
      if(c!=maxlen-2) ch=osGetChar(fh);
   }

   if(ch==13 || ch==10)
      buf[c++]=ch;

   buf[c]=0;

   if(ch==0)  messageend=TRUE;
   if(ch==-1) shortread=TRUE;

   return(c);
}

bool ReadPkt(uchar *pkt,struct osFileEntry *fe,bool bundled,bool (*handlefunc)(struct MemMessage *mm))
{
   osFile fh;
   struct Aka *tmpaka;
   struct ConfigNode *tmpcnode;
   ulong rlen,msgnum,msgoffset;
   uchar buf[200];
   uchar PktHeader[SIZE_PKTHEADER];
   uchar PktMsgHeader[SIZE_PKTMSGHEADER];
   struct Node4D PktOrig,PktDest;
   uchar *monthnames[]={"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","???"};
   struct MemMessage *mm;
   struct TextChunk *chunk;
   bool pkt_pw,pkt_4d,pkt_5d;

   shortread=FALSE;
   longread=FALSE;

   pkt_pw=FALSE;
   pkt_4d=FALSE;
   pkt_5d=FALSE;

   PktOrig.Zone=0;
   PktOrig.Net=0;
   PktOrig.Node=0;
   PktOrig.Point=0;

   PktDest.Zone=0;
   PktDest.Net=0;
   PktDest.Node=0;
   PktDest.Point=0;

   if(!(mm=mmAlloc()))
      return(FALSE);

   if(!(fh=osOpen(pkt,MODE_OLDFILE)))
   {
      LogWrite(1,SYSTEMERR,"Unable to open %s",pkt);
      mmFree(mm);
      return(TRUE);
   }

   if(osRead(fh,PktHeader,SIZE_PKTHEADER)!=SIZE_PKTHEADER)
   {
      LogWrite(1,TOSSINGERR,"Packet header in %s is too short",pkt);
      osClose(fh);
      mmFree(mm);
      BadFile(pkt,"Packet header is too short");
      return(TRUE);
   }

   if(getuword(PktHeader,PKTHEADER_PKTTYPE)!=0x0002)
   {
      LogWrite(1,TOSSINGERR,"%s is not a Type-2 packet",pkt);
      osClose(fh);
      mmFree(mm);
      BadFile(pkt,"Not a Type-2 packet");
      return(TRUE);
   }

   if(getuword(PktHeader,PKTHEADER_BAUD) == 2)
   {
      /* PktOrig och PktDest */

      PktOrig.Zone  = getuword(PktHeader,PKTHEADER_ORIGZONE);
      PktOrig.Net   = getuword(PktHeader,PKTHEADER_ORIGNET);
      PktOrig.Node  = getuword(PktHeader,PKTHEADER_ORIGNODE);
      PktOrig.Point = getuword(PktHeader,PKTHEADER_ORIGPOINT);

      PktDest.Zone  = getuword(PktHeader,PKTHEADER_DESTZONE);
      PktDest.Net   = getuword(PktHeader,PKTHEADER_DESTNET);
      PktDest.Node  = getuword(PktHeader,PKTHEADER_DESTNODE);
      PktDest.Point = getuword(PktHeader,PKTHEADER_DESTPOINT);


      pkt_5d=TRUE;
   }
   else
   {
      /* PktOrig och PktDest */

      if(getuword(PktHeader,PKTHEADER_ORIGZONE))
      {
         PktOrig.Zone = getuword(PktHeader,PKTHEADER_ORIGZONE);
         PktDest.Zone = getuword(PktHeader,PKTHEADER_DESTZONE);
      }
      else if(getuword(PktHeader,PKTHEADER_QORIGZONE))
      {
         PktOrig.Zone= getuword(PktHeader,PKTHEADER_QORIGZONE);
         PktDest.Zone= getuword(PktHeader,PKTHEADER_QDESTZONE);
      }
      else
      {
         PktOrig.Zone=0;
         PktDest.Zone=0;
      }

      PktOrig.Net  = getuword(PktHeader,PKTHEADER_ORIGNET);
      PktOrig.Node = getuword(PktHeader,PKTHEADER_ORIGNODE);
      PktDest.Net  = getuword(PktHeader,PKTHEADER_DESTNET);
      PktDest.Node = getuword(PktHeader,PKTHEADER_DESTNODE);

      if(PktHeader[PKTHEADER_CWVALIDCOPY] == PktHeader[PKTHEADER_CAPABILWORD+1] &&
         PktHeader[PKTHEADER_CWVALIDCOPY+1] == PktHeader[PKTHEADER_CAPABILWORD])
      {
         pkt_4d=TRUE;

         if(getuword(PktHeader,PKTHEADER_ORIGPOINT)!=0 && getuword(PktHeader,PKTHEADER_ORIGNET)==0xffff)
            PktOrig.Net = getuword(PktHeader,PKTHEADER_AUXNET);

         PktOrig.Point = getuword(PktHeader,PKTHEADER_ORIGPOINT);
         PktDest.Point = getuword(PktHeader,PKTHEADER_DESTPOINT);
      }
   }

   /* Check packet destination */

   if((config.cfg_Flags & CFG_CHECKPKTDEST) && !no_security)
   {
      for(tmpaka=(struct Aka *)config.AkaList.First;tmpaka;tmpaka=tmpaka->Next)
         if(Compare4D(&tmpaka->Node,&PktDest) == 0) break;

      if(!tmpaka)
      {
         LogWrite(1,TOSSINGERR,"Addressed to %u:%u/%u.%u, not to me",PktDest.Zone,PktDest.Net,PktDest.Node,PktDest.Point);
         osClose(fh);
         mmFree(mm);
         sprintf(buf,"Addressed to %u:%u/%u.%u, not to me",PktDest.Zone,PktDest.Net,PktDest.Node,PktDest.Point);
         BadFile(pkt,buf);
         return(TRUE);
      }
   }

   /* Fixa zone */

   if(PktOrig.Zone == 0)
      for(tmpcnode=(struct ConfigNode *)config.CNodeList.First;tmpcnode;tmpcnode=tmpcnode->Next)
      {
         if(Compare4D(&PktOrig,&tmpcnode->Node)==0)
         {
            PktOrig.Zone=tmpcnode->Node.Zone;
            break;
         }
      }

   if(PktDest.Zone == 0)
      for(tmpaka=(struct Aka *)config.AkaList.First;tmpaka;tmpaka=tmpaka->Next)
      {
         if(Compare4D(&PktDest,&tmpaka->Node)==0)
         {
            PktDest.Zone=tmpaka->Node.Zone;
            break;
         }
      }

   if(PktOrig.Zone == 0) PktOrig.Zone=config.cfg_DefaultZone;
   if(PktDest.Zone == 0) PktDest.Zone=config.cfg_DefaultZone;

   for(tmpcnode=(struct ConfigNode *)config.CNodeList.First;tmpcnode;tmpcnode=tmpcnode->Next)
      if(Compare4D(&PktOrig,&tmpcnode->Node)==0) break;

   if(tmpcnode)
   {
      if(tmpcnode->PacketPW[0]!=0 && PktHeader[PKTHEADER_PASSWORD]!=0)
         pkt_pw=TRUE;
   }

   buf[0]=0;

   if(pkt_pw) strcat(buf,", pw");
   if(pkt_4d) strcat(buf,", 4d");
   if(pkt_5d) strcat(buf,", 5d");

   if(buf[0] != 0)
      buf[0]='/';

   if(pkt_5d)
   {
      uchar domain[10];

      mystrncpy(domain,&PktHeader[PKTHEADER45_ORIGDOMAIN],9);

      LogWrite(1,ACTIONINFO,"Tossing %s (%luK) from %ld:%ld/%ld.%ld@%s%s",
                                                              fe->Name,
                                                              (fe->Size+512)/1024,
                                                              PktOrig.Zone,
                                                              PktOrig.Net,
                                                              PktOrig.Node,
                                                              PktOrig.Point,
                                                              domain,
                                                              buf);
   }
   else
   {
      int month;

      month=getuword(PktHeader,PKTHEADER_MONTH);

      if(month > 11)
         month=12;

      LogWrite(1,ACTIONINFO,"Tossing %s (%luK) from %ld:%ld/%ld.%ld (%02ld-%s-%02ld %02ld:%02ld:%02ld)%s",
         fe->Name,
         (fe->Size+512)/1024,
         PktOrig.Zone,
         PktOrig.Net,
         PktOrig.Node,
         PktOrig.Point,
         getuword(PktHeader,PKTHEADER_DAY),
         monthnames[month],
         getuword(PktHeader,PKTHEADER_YEAR) % 100,
         getuword(PktHeader,PKTHEADER_HOUR),
         getuword(PktHeader,PKTHEADER_MINUTE),
         getuword(PktHeader,PKTHEADER_SECOND),
         buf);
   }

   if(tmpcnode)
   {
      strncpy(buf,&PktHeader[PKTHEADER_PASSWORD],8);
      buf[8]=0;

      if(tmpcnode->PacketPW[0]!=0 && stricmp(buf,tmpcnode->PacketPW)!=0)
      {
         LogWrite(1,TOSSINGERR,"Wrong password");
         osClose(fh);
         mmFree(mm);
         BadFile(pkt,"Wrong password");
         return(TRUE);
      }
   }

   msgoffset=osFTell(fh);

   if(osRead(fh,PktMsgHeader,SIZE_PKTMSGHEADER) < 2)
   {
      LogWrite(1,TOSSINGERR,"Message header for msg #1 (offset %ld) is too short",msgoffset);
      osClose(fh);
      mmFree(mm);
      sprintf(buf,"Message header for msg #1 (offset %ld) is too short",msgoffset);
      BadFile(pkt,buf);
      return(TRUE);
   }

   msgnum=0;

   while(getuword(PktMsgHeader,PKTMSGHEADER_PKTTYPE) == 2)
   {
      msgnum++;

      printf("Message %lu              \x0d",msgnum);
      fflush(stdout);

      /* Init variables */

      mmClear(mm);

      mm->OrigNode.Net  = getuword(PktMsgHeader,PKTMSGHEADER_ORIGNET);
      mm->OrigNode.Node = getuword(PktMsgHeader,PKTMSGHEADER_ORIGNODE);

      mm->DestNode.Net  = getuword(PktMsgHeader,PKTMSGHEADER_DESTNET);
      mm->DestNode.Node = getuword(PktMsgHeader,PKTMSGHEADER_DESTNODE);

      mm->DestNode.Zone=PktDest.Zone;
      mm->DestNode.Zone=PktOrig.Zone;

      mm->Attr=getuword(PktMsgHeader,PKTMSGHEADER_ATTR);
      mm->Cost=getuword(PktMsgHeader,PKTMSGHEADER_COST);

      Copy4D(&mm->PktOrig,&PktOrig);
      Copy4D(&mm->PktDest,&PktDest);

      if(no_security)
         mm->no_security=TRUE;

      /* Get header strings */

      ReadNull(mm->DateTime,20,fh);
      ReadNull(mm->To,36,fh);
      ReadNull(mm->From,36,fh);
      ReadNull(mm->Subject,72,fh);

      /* Corrupt packet? */

      if(shortread)
      {
         LogWrite(1,TOSSINGERR,"Message header for msg #%lu (offset %ld) is short",msgnum,msgoffset);
         sprintf(buf,"Message header for msg #%lu (offset %ld) is short",msgnum,msgoffset);
         osClose(fh);
         mmFree(mm);
         BadFile(pkt,buf);
         return(TRUE);
      }

      if(longread)
      {
         LogWrite(1,TOSSINGERR,"Header strings too long in msg #%lu (offset %ld)",msgnum,msgoffset);
         sprintf(buf,"Header strings too long in msg #%lu (offset %ld)",msgnum,msgoffset);
         osClose(fh);
         mmFree(mm);
         BadFile(pkt,buf);
         return(TRUE);
      }

      /* Start reading message text */

      messageend=FALSE;

      rlen=ReadCR(buf,200,fh);

      /* Echomail or netmail? */

      if(strncmp(buf,"AREA:",5)==0)
      {
         mystrncpy(mm->Area,&buf[5],40);
         strip(mm->Area); /* Strip spaces from area name */
      }
      else
      {
         if(!mmAddLine(mm,buf))
         {
            osClose(fh);
            mmFree(mm);
            return(FALSE);
         }
      }

      /* Get rest of text */

      while(!messageend)
      {
         rlen=ReadCR(buf,200,fh);

         if(shortread)
         {
            osClose(fh);
            mmFree(mm);
            LogWrite(1,TOSSINGERR,"Got unexpected EOF when reading msg #%lu (offset %ld)",msgnum,msgoffset);
            sprintf(buf,"Got unexpected EOF when reading msg #%lu (offset %ld)",msgnum,msgoffset);
            BadFile(pkt,buf);
            return(TRUE);
         }

         if(buf[0])
         {
            if(!mmAddLine(mm,buf))
            {
               osClose(fh);
               mmFree(mm);
               return(FALSE);
            }
         }
      }

      /* Stats */
      
      for(tmpcnode=(struct ConfigNode *)config.CNodeList.First;tmpcnode;tmpcnode=tmpcnode->Next)
         if(Compare4D(&tmpcnode->Node,&mm->PktOrig)==0) break;
         
      if(tmpcnode)
      {
         ulong size;
         
         size=0;

         for(chunk=(struct TextChunk *)mm->TextChunks.First;chunk;chunk=chunk->Next)
            size+=chunk->Length;

         if(mm->Area[0])
         {
            tmpcnode->GotEchomails++;
            tmpcnode->GotEchomailBytes+=size;
         }
         else
         {
            tmpcnode->GotNetmails++;
            tmpcnode->GotNetmailBytes+=size;
         }
      }
      
      if(ctrlc)
      {
         osClose(fh);
         mmFree(mm);
         return(FALSE);
      }

      if(!(*handlefunc)(mm))
      {
         osClose(fh);
         mmFree(mm);
         return(FALSE);
      }

      msgoffset=osFTell(fh);

      if(osRead(fh,PktMsgHeader,SIZE_PKTMSGHEADER) < 2)
      {
         LogWrite(1,TOSSINGERR,"Packet header too short for msg #%lu (offset %ld)",msgnum+1,msgoffset);
         osClose(fh);
         mmFree(mm);
         sprintf(buf,"Packet header too short for msg #%lu (offset %ld)",msgnum+1,msgoffset);
         BadFile(pkt,buf);
         return(TRUE);
      }
   }

   if(getuword(PktMsgHeader,PKTMSGHEADER_PKTTYPE)!=0)
   {
      osClose(fh);
      mmFree(mm);
      LogWrite(1,TOSSINGERR,"Unknown message type %lu for message #%lu (offset %ld)",getuword(PktMsgHeader,PKTMSGHEADER_PKTTYPE),msgnum+1,msgoffset);
      sprintf(buf,"Unknown message type %u for message #%lu (offset %ld)",getuword(PktMsgHeader,PKTMSGHEADER_PKTTYPE),msgnum+1,msgoffset);
      BadFile(pkt,buf);
      return(TRUE);
   }

   printf("                      \x0d");
   fflush(stdout);

   osClose(fh);

   return(TRUE);
}

/*************************************************************************/
/*                                                                       */
/*                               Write Pkt                               */
/*                                                                       */
/*************************************************************************/

struct Pkt
{
   struct Pkt *Next;
   osFile fh;
   ulong hexnum;
   ulong Len;
   ushort Type;
   struct Node4D Dest;
   struct Node4D Orig;
};

void pktWrite(struct Pkt *pkt,uchar *buf,ulong len)
{
   osWrite(pkt->fh,buf,len);
   pkt->Len+=len;
}

void WriteNull(struct Pkt *pkt,uchar *str)
{
   pktWrite(pkt,str,(ulong)(strlen(str)+1));
}

struct Pkt *FindPkt(struct Node4D *node,struct Node4D *mynode,ushort type)
{
   struct Pkt *pkt;

   for(pkt=(struct Pkt *)PktList.First;pkt;pkt=pkt->Next)
      if(Compare4D(node,&pkt->Dest)==0 && Compare4D(mynode,&pkt->Orig)==0 && type==pkt->Type)
         return(pkt);

   return(NULL);
}

time_t lastt;
ulong serial;

struct Pkt *CreatePkt(struct Node4D *dest,struct ConfigNode *node,struct Aka *aka,ushort type)
{
   uchar buf[100],buf2[100];
   struct Pkt *pkt;
   ulong num,c;
   time_t t;
   struct tm *tp;
   uchar PktHeader[SIZE_PKTHEADER];
      
   do
   {
      t=time(NULL);
      if(t == lastt) serial++;
      else serial=0;
      if(serial == 256) serial=0;
      lastt=t; 
      num = (t<<8) + serial;
      sprintf(buf2,"%08lx.newpkt",num);
      
      MakeFullPath(config.cfg_PacketCreate,buf2,buf,100);
   } while(osExists(buf));

   if(!(pkt=(struct Pkt *)osAllocCleared(sizeof(struct Pkt))))
   {
      nomem=TRUE;
      return(NULL);
   }

   if(!(pkt->fh=osOpen(buf,MODE_NEWFILE)))
   {
      LogWrite(1,SYSTEMERR,"Unable to create packet %s",buf);
      osFree(pkt);
      return(NULL);
   }

   pkt->hexnum=num;
   pkt->Type=type;
   Copy4D(&pkt->Dest,dest);
   Copy4D(&pkt->Orig,&aka->Node);

   putuword(PktHeader,PKTHEADER_ORIGNODE,aka->Node.Node);
   putuword(PktHeader,PKTHEADER_DESTNODE,dest->Node);

   time(&t);
   tp=localtime(&t);

   putuword(PktHeader,PKTHEADER_DAY,tp->tm_mday);
   putuword(PktHeader,PKTHEADER_MONTH,tp->tm_mon);
   putuword(PktHeader,PKTHEADER_YEAR,tp->tm_year+1900);
   putuword(PktHeader,PKTHEADER_HOUR,tp->tm_hour);
   putuword(PktHeader,PKTHEADER_MINUTE,tp->tm_min);
   putuword(PktHeader,PKTHEADER_SECOND,tp->tm_sec);

   putuword(PktHeader,PKTHEADER_BAUD,0);
   putuword(PktHeader,PKTHEADER_PKTTYPE,2);
   putuword(PktHeader,PKTHEADER_ORIGNET,aka->Node.Net);
   putuword(PktHeader,PKTHEADER_DESTNET,dest->Net);
   PktHeader[PKTHEADER_PRODCODELOW]=0xfe;
   PktHeader[PKTHEADER_REVMAJOR]=VERSION_MAJOR;
   putuword(PktHeader,PKTHEADER_QORIGZONE,aka->Node.Zone);
   putuword(PktHeader,PKTHEADER_QDESTZONE,dest->Zone);
   putuword(PktHeader,PKTHEADER_AUXNET,0);
   putuword(PktHeader,PKTHEADER_CWVALIDCOPY,0x0100);
   PktHeader[PKTHEADER_PRODCODEHIGH]=0;
   PktHeader[PKTHEADER_REVMINOR]=VERSION_MINOR;
   putuword(PktHeader,PKTHEADER_CAPABILWORD,0x0001);
   putuword(PktHeader,PKTHEADER_ORIGZONE,aka->Node.Zone);
   putuword(PktHeader,PKTHEADER_DESTZONE,dest->Zone);
   putuword(PktHeader,PKTHEADER_ORIGPOINT,aka->Node.Point);
   putuword(PktHeader,PKTHEADER_DESTPOINT,dest->Point);
   PktHeader[PKTHEADER_PRODDATA]=0;
   PktHeader[PKTHEADER_PRODDATA+1]=0;
   PktHeader[PKTHEADER_PRODDATA+2]=0;
   PktHeader[PKTHEADER_PRODDATA+3]=0;

   for(c=0;c<8;c++)
      PktHeader[PKTHEADER_PASSWORD+c]=0;

   if(node)
      strncpy(&PktHeader[PKTHEADER_PASSWORD],node->PacketPW,8);

   pktWrite(pkt,PktHeader,SIZE_PKTHEADER);

   if(diskfull)
   {
      osClose(pkt->fh);
      osFree(pkt);
      return(NULL);
   }

   return(pkt);
}

void FinishPacket(struct Pkt *pkt)
{
   pktWrite(pkt,"",1);
   pktWrite(pkt,"",1);
   osClose(pkt->fh);

   if(pkt->hexnum)
   {
      uchar oldname[200],newname[200],buf1[100],buf2[100],*typestr;

      /* Create packet name */

      switch(pkt->Type)
      {
         case PKTS_HOLD:      typestr="Hold";
                              break;
         case PKTS_NORMAL:    typestr="Normal";
                              break;
         case PKTS_DIRECT:    typestr="Direct";
                              break;
         case PKTS_CRASH:     typestr="Crash";
                              break;
         case PKTS_ECHOMAIL:  typestr="Echomail";
                              break;
      }

      sprintf(buf1,"%08lx.newpkt",pkt->hexnum);

      sprintf(buf2,"%08lx_%s_%d_%d_%d_%d.newpkt",
         pkt->hexnum,
         typestr,
         pkt->Dest.Zone,
         pkt->Dest.Net,
         pkt->Dest.Node,
         pkt->Dest.Point);

      MakeFullPath(config.cfg_PacketCreate,buf1,oldname,200);
      MakeFullPath(config.cfg_PacketCreate,buf2,newname,200);
      osRename(oldname,newname);
   }
   
   jbFreeNode(&PktList,(struct jbNode *)pkt);
}

void ClosePackets(void)
{
   struct Pkt *pkt,*pkt2;

   pkt=(struct Pkt *)PktList.First;

   while(pkt)
   {
      pkt2=pkt->Next;
      FinishPacket(pkt);
      pkt=pkt2;
   }
}

bool WriteMsgHeader(struct Pkt *pkt,struct MemMessage *mm)
{
   uchar PktMsgHeader[SIZE_PKTMSGHEADER];

   putuword(PktMsgHeader,PKTMSGHEADER_PKTTYPE,0x0002);
   putuword(PktMsgHeader,PKTMSGHEADER_ORIGNODE,mm->OrigNode.Node);
   putuword(PktMsgHeader,PKTMSGHEADER_DESTNODE,mm->DestNode.Node);
   putuword(PktMsgHeader,PKTMSGHEADER_ORIGNET,mm->OrigNode.Net);
   putuword(PktMsgHeader,PKTMSGHEADER_DESTNET,mm->DestNode.Net);
   putuword(PktMsgHeader,PKTMSGHEADER_ATTR,mm->Attr);
   putuword(PktMsgHeader,PKTMSGHEADER_COST,mm->Cost);

   pktWrite(pkt,PktMsgHeader,SIZE_PKTMSGHEADER);

   WriteNull(pkt,mm->DateTime);
   WriteNull(pkt,mm->To);
   WriteNull(pkt,mm->From);
   WriteNull(pkt,mm->Subject);

   if(diskfull)
      return(FALSE);

   return(TRUE);
}

bool WritePath(struct Pkt *pkt,struct jbList *list)
{
   ushort c;
   struct Path *path;

   for(path=(struct Path *)list->First;path;path=path->Next)
      for(c=0;c<path->Paths;c++)
         if(path->Path[c][0]!=0)
         {
            pktWrite(pkt,"\x01PATH: ",7);
            pktWrite(pkt,path->Path[c],(ulong)strlen(path->Path[c]));
            pktWrite(pkt,"\x0d",1);
         }

   if(diskfull)
      return(FALSE);

   return(TRUE);
}

bool WriteSeenBy(struct Pkt *pkt,struct jbList *list)
{
   uchar *buf;

   if(!(buf=mmMakeSeenByBuf(list)))
      return(FALSE);

   pktWrite(pkt,buf,(ulong)strlen(buf));

   osFree(buf);

   return(TRUE);
}

bool WriteEchoMail(struct MemMessage *mm,struct ConfigNode *node, struct Aka *aka)
{
   uchar buf[100];
   struct Pkt *pkt;
   struct TextChunk  *chunk;
   ulong size;

   toss_written++;

   mm->Type=PKTS_ECHOMAIL;

   size=0;

   for(chunk=(struct TextChunk *)mm->TextChunks.First;chunk;chunk=chunk->Next)
      size+=chunk->Length;

   node->SentEchomails++;
   node->SentEchomailBytes+=size;

   pkt=FindPkt(&node->Node,&aka->Node,mm->Type);

   if(!pkt || (pkt && config.cfg_MaxPktSize!=0 && pkt->Len > config.cfg_MaxPktSize))
   {
      if(pkt) FinishPacket(pkt);

      if(!(pkt=CreatePkt(&node->Node,node,aka,mm->Type)))
      {
         return(FALSE);
      }

      jbAddNode(&PktList,(struct jbNode *)pkt);
   }

   Copy4D(&mm->DestNode,&node->Node);
   Copy4D(&mm->OrigNode,&aka->Node);

   if(!WriteMsgHeader(pkt,mm))
      return(FALSE);

   sprintf(buf,"AREA:%s\x0d",mm->Area);

   pktWrite(pkt,buf,(ulong)strlen(buf));

   if(diskfull)
      return(FALSE);

   for(chunk=(struct TextChunk *)mm->TextChunks.First;chunk;chunk=chunk->Next)
   {
      pktWrite(pkt,chunk->Data,chunk->Length);
      
      if(diskfull)
         return(FALSE);
   }

   if(node->Node.Zone != aka->Node.Zone || (node->Flags & NODE_TINYSEENBY))
   {
      struct jbList seenbylist;
      struct Nodes2D *seenby;

      if(!(seenby=(struct Nodes2D *)osAlloc(sizeof(struct Nodes2D))))
      {
         nomem=TRUE;
         return(FALSE);
      }

      seenby->Nodes=0;
      seenby->Next=NULL;

      jbNewList(&seenbylist);
      jbAddNode(&seenbylist,(struct jbNode *)seenby);

      if(node->Node.Point == 0)
         if(!mmAddNodes2DList(&seenbylist,node->Node.Net,node->Node.Node))
         {
            jbFreeList(&seenbylist);
            return(FALSE);
         }

      if(aka->Node.Point == 0)
         if(!mmAddNodes2DList(&seenbylist,aka->Node.Net,aka->Node.Node))
         {
            jbFreeList(&seenbylist);
            return(FALSE);
         }

      if(!mmSortNodes2D(&mm->SeenBy))
      {
         jbFreeList(&seenbylist);
         return(FALSE);
      }

      if(!WriteSeenBy(pkt,&mm->SeenBy))
      {
         jbFreeList(&seenbylist);
         return(FALSE);
      }

      jbFreeList(&seenbylist);
   }
   else if(!(node->Flags & NODE_NOSEENBY))
   {
      if(!mmSortNodes2D(&mm->SeenBy))
         return(FALSE);

      if(!WriteSeenBy(pkt,&mm->SeenBy))
         return(FALSE);
   }

   if(!WritePath(pkt,&mm->Path))
      return(FALSE);

   pktWrite(pkt,"",1);

   return(TRUE);
}

bool WriteNetMail(struct MemMessage *mm,struct Node4D *dest,struct Aka *aka)
{
   struct Pkt *pkt;
   struct ConfigNode *cnode;
   struct TextChunk *chunk;
   ulong size;

   toss_written++;

   for(cnode=(struct ConfigNode *)config.CNodeList.First;cnode;cnode=cnode->Next)
      if(Compare4D(&cnode->Node,dest)==0) break;

   if(cnode)
   {
      /* Calculate size */

      size=0;

      for(chunk=(struct TextChunk *)mm->TextChunks.First;chunk;chunk=chunk->Next)
         size+=chunk->Length;

      cnode->SentNetmails++;
      cnode->SentNetmailBytes+=size;

      if(cnode->Flags & NODE_PACKNETMAIL)
         if(mm->Type == PKTS_NORMAL) mm->Type=PKTS_ECHOMAIL;
   }

   pkt=FindPkt(dest,&aka->Node,mm->Type);

   if(!pkt || (pkt && config.cfg_MaxPktSize!=0 && pkt->Len > config.cfg_MaxPktSize))
   {
      if(pkt) FinishPacket(pkt);

      if(!(pkt=CreatePkt(dest,cnode,aka,mm->Type)))
      {
         return(FALSE);
      }

      jbAddNode(&PktList,(struct jbNode *)pkt);
   }

   if(!WriteMsgHeader(pkt,mm))
      return(FALSE);

   for(chunk=(struct TextChunk *)mm->TextChunks.First;chunk;chunk=chunk->Next)
   {
      pktWrite(pkt,chunk->Data,chunk->Length);

      if(diskfull)
         return(FALSE);
   }

   pktWrite(pkt,"",1);

   return(TRUE);
}

