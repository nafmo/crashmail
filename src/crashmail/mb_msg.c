#include "crashmail.h"

struct msg_Area
{
   struct msg_Area *Next;
   struct Area *area;
   ulong LowMsg;
   ulong HighMsg;
   ulong OldHighWater;
   ulong HighWater;
};

bool msg_GetHighLowMsg(struct msg_Area *area);
bool msg_WriteHighWater(struct msg_Area *area);
bool msg_WriteMSG(struct MemMessage *mm,uchar *file);
ulong msg_ReadCR(uchar *buf, ulong maxlen, osFile fh);
bool msg_ExportMSGNum(struct Area *area,ulong num,bool (*handlefunc)(struct MemMessage *mm));

struct jbList msg_AreaList;

bool msg_messageend;
bool msg_shortread;

struct msg_Area *msg_getarea(struct Area *area)
{
   struct msg_Area *ma;

   /* Check if area already exists */

   for(ma=(struct msg_Area *)msg_AreaList.First;ma;ma=ma->Next)
      if(ma->area == area) return(ma);

   /* This is the first time we use this area */ 

   if(!(ma=osAllocCleared(sizeof(struct msg_Area))))
   {
      nomem=TRUE;
      return(FALSE);
   }

   jbAddNode(&msg_AreaList,(struct jbNode *)ma);
   ma->area=area;

   if(!msg_GetHighLowMsg(ma))
      return(FALSE);

   return(ma);
}

bool msg_beforefunc(void)
{
   jbNewList(&msg_AreaList);

   return(TRUE);
}

bool msg_afterfunc(bool success)
{
   struct msg_Area *ma;

   if(success && (config.cfg_msg_Flags & CFG_MSG_HIGHWATER))
      for(ma=(struct msg_Area *)msg_AreaList.First;ma;ma=ma->Next)
         if(ma->HighWater != ma->OldHighWater)
            msg_WriteHighWater(ma);

   return(TRUE);
}

bool msg_importfunc(struct MemMessage *mm,struct Area *area)
{
   uchar buf[200],buf2[20];
   struct msg_Area *ma;

   if(!(ma=msg_getarea(area)))
      return(FALSE);

   ma->HighMsg++;

   sprintf(buf2,"%lu.msg",ma->HighMsg);
   MakeFullPath(ma->area->Path,buf2,buf,200);

   while(osExists(buf))
   {
      ma->HighMsg++;
      sprintf(buf2,"%lu.msg",ma->HighMsg);
      MakeFullPath(ma->area->Path,buf2,buf,200);
   }

   return msg_WriteMSG(mm,buf);
}

bool msg_rescanfunc(struct Area *area,ulong max,bool (*handlefunc)(struct MemMessage *mm))
{
   ulong start;
   struct msg_Area *ma;

   if(!(ma=msg_getarea(area)))
      return(FALSE);

   start=ma->LowMsg;

   if(max !=0 && ma->HighMsg-start+1 > max)
      start=ma->HighMsg-max+1;

   while(start <= ma->HighMsg)
   {
      if(!msg_ExportMSGNum(area,start,handlefunc))
         return(FALSE);

      if(ctrlc)
         return(FALSE);

      start++;
   }

   return(TRUE);
}

bool msg_exportfunc(struct Area *area,bool (*handlefunc)(struct MemMessage *mm))
{
   ulong start;
   uchar buf[200];
   struct StoredMsg Msg;
   osFile fh;
   struct msg_Area *ma;

   if(!(ma=msg_getarea(area)))
      return(FALSE);

   if(config.cfg_msg_Flags & CFG_MSG_HIGHWATER)
   {
      if(ma->HighWater == 0)
      {
         MakeFullPath(area->Path,"1.msg",buf,200);

         if((fh=osOpen(buf,MODE_OLDFILE)))
         {
            if((osRead(fh,&Msg,sizeof(struct StoredMsg))==sizeof(struct StoredMsg)))
            {
               ma->HighWater    = Msg.ReplyTo;
               ma->OldHighWater = Msg.ReplyTo;
            }
            osClose(fh);
         }
      }
   }
  
   if(ma->HighWater) start=ma->HighWater+1;
   else              start=ma->LowMsg;

   if(start<ma->LowMsg)
      start=ma->LowMsg;

   while(start <= ma->HighMsg)
   {
      if(!msg_ExportMSGNum(area,start,handlefunc))
         return(FALSE);

      if(ctrlc)
         return(FALSE);

      start++;
   }

   ma->HighWater=start-1;

   return(TRUE);
}

bool msg_ExportMSGNum(struct Area *area,ulong num,bool (*handlefunc)(struct MemMessage *mm))
{
   ulong rlen;
   uchar buf[200],buf2[50];
   bool kludgeadd;
   osFile fh;
   struct StoredMsg Msg;
   struct MemMessage *mm;
	ushort oldattr;
	
   if(!(mm=mmAlloc()))
      return(FALSE);

   sprintf(buf2,"%lu.msg",num);
   MakeFullPath(area->Path,buf2,buf,200);

   if(!(fh=osOpen(buf,MODE_OLDFILE)))
   {
      /* Message doesn't exist */
      return(TRUE);
   }

   if(osRead(fh,&Msg,sizeof(struct StoredMsg))!=sizeof(struct StoredMsg))
   {
      LogWrite(1,TOSSINGERR,"Unexpected EOF while reading %s, message ignored",buf);
      osClose(fh);
      return(TRUE);
   }

   if((Msg.Attr & FLAG_SENT) && !isrescanning)
   {
      /* Message already sent */
      osClose(fh);
      return(TRUE);
   }

   mm->OrigNode.Net=Msg.OrigNet;
   mm->OrigNode.Node=Msg.OrigNode;
   mm->DestNode.Net=Msg.DestNet;
   mm->DestNode.Node=Msg.DestNode;

   if(area->Flags & AREA_NETMAIL)
      strcpy(mm->Area,"");

   else
      strcpy(mm->Area,area->Tagname);

   mystrncpy(mm->To,Msg.To,36);
   mystrncpy(mm->From,Msg.From,36);
   mystrncpy(mm->Subject,Msg.Subject,72);

   mystrncpy(mm->DateTime,Msg.DateTime,20);

   oldattr = Msg.Attr;

   mm->Attr = Msg.Attr & (FLAG_PVT|FLAG_CRASH|FLAG_FILEATTACH|FLAG_FILEREQ|FLAG_RREQ|FLAG_IRRR|FLAG_AUDIT|FLAG_HOLD);
   mm->Cost = Msg.Cost;

   kludgeadd=FALSE;
   msg_messageend=FALSE;
   msg_shortread=FALSE;

   do
   {
      rlen=msg_ReadCR(buf,200,fh);

      if(buf[0]!=1 && buf[0]!=10 && !kludgeadd)
      {
         kludgeadd=TRUE;

         if((config.cfg_Flags & CFG_ADDTID) && !isrescanning)
            AddTID(mm);

         if(isrescanning)
         {
            sprintf(buf2,"\x01RESCANNED %u:%u/%u.%u\x0d",area->Aka->Node.Zone,
                                                         area->Aka->Node.Net,
                                                         area->Aka->Node.Node,
                                                         area->Aka->Node.Point);
            mmAddLine(mm,buf2);
         }

         if(mm->Area[0]==0)
         {
            if(mm->DestNode.Zone == 0 || mm->OrigNode.Zone == 0)
            {
               /* No INTL line and no zone in header */
               mm->DestNode.Zone=area->Aka->Node.Zone;
               mm->OrigNode.Zone=area->Aka->Node.Zone;
               Msg.DestZone=area->Aka->Node.Zone;
               Msg.OrigZone=area->Aka->Node.Zone;

               if(config.cfg_Flags & CFG_FORCEINTL)
               {
                  sprintf(buf2,"\x01INTL %u:%u/%u %u:%u/%u\x0d",Msg.DestZone,Msg.DestNet,Msg.DestNode,
                                                                Msg.OrigZone,Msg.OrigNet,Msg.OrigNode);

                  mmAddLine(mm,buf2);
               }
            }
         }
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

   } while(!msg_messageend && !msg_shortread);

   osClose(fh);

   mm->msgnum=num;

   if(!(*handlefunc)(mm))
   {
      mmFree(mm);
      return(FALSE);
   }

   if(!isrescanning)
   {
      scan_total++;

      sprintf(buf2,"%lu.msg",num);
      MakeFullPath(area->Path,buf2,buf,200);

		if((config.cfg_Flags & CFG_ALLOWKILLSENT) && (oldattr & FLAG_KILLSENT) && (area->Flags & AREA_NETMAIL))
		{
			/* Delete message with KILLSENT flag */
			
			LogWrite(2,TOSSINGINFO,"Deleting message with KILLSENT flag");
		   osDelete(buf);
		}
		else
		{
		   Msg.Attr|=FLAG_SENT;

   	   if(config.cfg_msg_Flags & CFG_MSG_WRITEBACK)
      	{
         	mm->Attr=Msg.Attr;
	         msg_WriteMSG(mm,buf);
   	   }
      	else
	      {
   	      if((fh=osOpen(buf,MODE_READWRITE)))
      	   {
         	   osWrite(fh,&Msg,sizeof(struct StoredMsg));
            	osClose(fh);
	         }
   	   }
		}
   }

   mmFree(mm);

   return(TRUE);
}

ulong msg_templowmsg;
ulong msg_temphighmsg;

void msg_scandirfunc(uchar *file)
{
   if(strlen(file) > 4)
   {
      if(stricmp(&file[strlen(file)-4],".msg")==0)
      {
         if(atol(file) > msg_temphighmsg)
            msg_temphighmsg = atol(file);

         if(atol(file) < msg_templowmsg || msg_templowmsg==0 ||msg_templowmsg==1)
            if(atol(file) >= 2 ) msg_templowmsg=atol(file);
      }
   }
}

bool msg_GetHighLowMsg(struct msg_Area *area)
{
   if(!osExists(area->area->Path))
   {
      LogWrite(2,SYSTEMINFO,"Creating directory \"%s\"",area->area->Path);

      if(!osMkDir(area->area->Path))
      {
         LogWrite(1,SYSTEMERR,"Unable to create directory");
         return(FALSE);
      }
   }

   msg_templowmsg=0;
   msg_temphighmsg=0;

   if(!osScanDir(area->area->Path,msg_scandirfunc))
   {
      LogWrite(1,SYSTEMERR,"Failed to scan directory %s",area->area->Path);
      return(FALSE);
   }

   area->HighMsg=msg_temphighmsg;
   area->LowMsg=msg_templowmsg;

   if(area->HighMsg==0)
      area->HighMsg=1;

   if(area->LowMsg==0 || area->LowMsg==1)
      area->LowMsg=2;

   return(TRUE);
}

bool msg_WriteHighWater(struct msg_Area *area)
{
   osFile fh;
   uchar buf[200];
   struct StoredMsg Msg;

   if(area->HighWater > 65535)
   {
      LogWrite(1,TOSSINGERR,"Warning: Highwater mark in %s exceeds 65535, cannot store in 1.msg",
         area->area->Tagname);

      return(TRUE);
   }

   strcpy(Msg.From,"CrashMail II");
   strcpy(Msg.To,"All");
   strcpy(Msg.Subject,"HighWater mark");

   MakeFidoDate(time(NULL),Msg.DateTime);

   Msg.TimesRead=0;
   Msg.DestNode=0;
   Msg.OrigNode=0;
   Msg.Cost=0;
   Msg.OrigNet=0;
   Msg.DestNet=0;
   Msg.DestZone=0;
   Msg.OrigZone=0;
   Msg.OrigPoint=0;
   Msg.DestPoint=0;
   Msg.ReplyTo=area->HighWater;
   Msg.Attr=FLAG_SENT | FLAG_PVT;
   Msg.NextReply=0;

   MakeFullPath(area->area->Path,"1.msg",buf,200);

   if(!(fh=osOpen(buf,MODE_NEWFILE)))
   {
      LogWrite(1,TOSSINGERR,"Failed to write Highwater mark to %s",buf);
      return(FALSE);
   }

   osWrite(fh,&Msg,sizeof(struct StoredMsg));
   osWrite(fh,"",1);
   osClose(fh);

   return(TRUE);
}

bool msg_WriteMSG(struct MemMessage *mm,uchar *file)
{
   struct StoredMsg Msg;
   struct TextChunk *chunk;
   struct Path *path;
   osFile fh;
   int c;
   
   strcpy(Msg.From,mm->From);
   strcpy(Msg.To,mm->To);
   strcpy(Msg.Subject,mm->Subject);
   strcpy(Msg.DateTime,mm->DateTime);

   Msg.TimesRead=0;
   Msg.ReplyTo=0;
   Msg.NextReply=0;
   Msg.Cost= mm->Cost;
   Msg.Attr= mm->Attr | FLAG_SENT;

   if(mm->Area[0]==0)
   {
      Msg.DestZone   =  mm->DestNode.Zone;
      Msg.DestNet    =  mm->DestNode.Net;
      Msg.DestNode   =  mm->DestNode.Node;
      Msg.DestPoint  =  mm->DestNode.Point;

      Msg.OrigZone   =  mm->OrigNode.Zone;
      Msg.OrigNet    =  mm->OrigNode.Net;
      Msg.OrigNode   =  mm->OrigNode.Node;
      Msg.OrigPoint  =  mm->OrigNode.Point;
   }
   else
   {
      Msg.DestZone   =  0;
      Msg.DestNet    =  0;
      Msg.DestNode   =  0;
      Msg.DestPoint  =  0;

      Msg.OrigZone   =  0;
      Msg.OrigNet    =  0;
      Msg.OrigNode   =  0;
      Msg.OrigPoint  =  0;
   }

   if(!(fh=osOpen(file,MODE_NEWFILE)))
   {
      printf("Failed to write to %s\n",file);
      return(FALSE);
   }

   /* Write header */

   osWrite(fh,&Msg,sizeof(struct StoredMsg));

   /* Write text */

   for(chunk=(struct TextChunk *)mm->TextChunks.First;chunk;chunk=chunk->Next)
      osWrite(fh,chunk->Data,chunk->Length);

   /* Write seen-by */

   if((config.cfg_Flags & CFG_IMPORTSEENBY) && mm->Area[0]!=0)
   {
      uchar *sbbuf;

      if(!(sbbuf=mmMakeSeenByBuf(&mm->SeenBy)))
      {
         osClose(fh);
         return(FALSE);
      }

      if(sbbuf[0])
         osWrite(fh,sbbuf,(ulong)strlen(sbbuf));

      osFree(sbbuf);
   }

   /* Write path */

   for(path=(struct Path *)mm->Path.First;path;path=path->Next)
      for(c=0;c<path->Paths;c++)
         if(path->Path[c][0]!=0)
         {
            osWrite(fh,"\x01PATH: ",7);
            osWrite(fh,path->Path[c],(ulong)strlen(path->Path[c]));
            osWrite(fh,"\x0d",1);
         }

   osPutChar(fh,0);

   osClose(fh);

   if(diskfull)
      return(FALSE);

   return(TRUE);
}

ulong msg_ReadCR(uchar *buf, ulong maxlen, osFile fh)
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

   if(ch==0)  msg_messageend=TRUE;
   if(ch==-1) msg_shortread=TRUE;

   return(c);
}
