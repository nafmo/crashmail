#include "crashmail.h"

extern uchar *config_version;

bool CorrectFlags(uchar *flags)
{
   ulong c;

   for(c=0;c<strlen(flags);c++)
   {
      flags[c]=toupper(flags[c]);

      if(flags[c]<'A' || flags[c]>'Z')
         return(FALSE);
   }

   return(TRUE);
}

uchar cfgbuf[4000];

bool ReadConfig(uchar *filename,struct Config *cfg,short *seconderr,ulong *cfgline,uchar *cfgerr)
{
   uchar buf2[200],cfgword[30];
   ulong c,d,jbcpos;
   osFile cfgfh;
   
   struct Aka           *tmpaka,    *LastAka=NULL;
   struct ConfigNode    *tmpnode,   *LastCNode=NULL;
   struct Area          *tmparea,   *LastArea=NULL;
   struct Packer        *tmppacker, *LastPacker=NULL;
   struct Route         *tmproute;
   struct ImportNode    *tmpinode;
   struct PatternNode   *tmppatternnode;
   struct Change        *tmpchange;
   struct Remap         *tmpremap;
   struct RemapNode     *tmpremapnode;
   struct Robot         *tmprobot;
   struct AreaFixName   *tmpareafixname;
   struct Arealist      *tmparealist;

   struct AddNode    *tmpaddnode;
   struct RemNode    *tmpremnode;
   struct TossNode   *tmptnode;
   struct BannedNode *tmpbnode;
   struct Node4D     tmp4d;

   ushort flags;

   cfg->changed=FALSE;
   mystrncpy(cfg->filename,filename,100);

   *cfgline=0;

   if(!(cfgfh=osOpen(filename,MODE_OLDFILE)))
   {
      *seconderr=READCONFIG_NOT_FOUND;
      return(FALSE);
   }

   *seconderr=READCONFIG_INVALID;

   while(osFGets(cfgfh,cfgbuf,4000))
   {
      jbcpos=0;
      (*cfgline)++;

      jbstrcpy(cfgword,cfgbuf,30,&jbcpos);

      if(stricmp(cfgword,"AKA")==0)
      {
         if(!(tmpaka=(struct Aka *)osAllocCleared(sizeof(struct Aka))))
         {
            *seconderr=READCONFIG_NO_MEM;
            osClose(cfgfh);
            return(FALSE);
         }

         jbAddNode(&cfg->AkaList,(struct jbNode *)tmpaka);
         LastAka=tmpaka;

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(Parse4D(buf2,&LastAka->Node)))
         {
            sprintf(cfgerr,"Invalid node number \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         jbNewList(&LastAka->AddList);
         jbNewList(&LastAka->RemList);
      }
      else if(stricmp(cfgword,"ADDNODE")==0)
      {
         if(!LastAka)
         {
            strcpy(cfgerr,"No previous AKA line");
            osClose(cfgfh);
            return(FALSE);
         }

         while(jbstrcpy(buf2,cfgbuf,100,&jbcpos))
         {
            if(!(tmpaddnode=(struct AddNode *)osAllocCleared(sizeof(struct AddNode))))
            {
               *seconderr=READCONFIG_NO_MEM;
               osClose(cfgfh);
               return(FALSE);
            }

            jbAddNode(&LastAka->AddList,(struct jbNode *)tmpaddnode);

            if(!(Parse4D(buf2,&tmpaddnode->Node)))
            {
               sprintf(cfgerr,"Invalid node number \"%s\"",buf2);
               osClose(cfgfh);
               return(FALSE);
            }
         }
      }
      else if(stricmp(cfgword,"REMNODE")==0)
      {
         if(!LastAka)
         {
            strcpy(cfgerr,"No previous AKA line");
            osClose(cfgfh);
            return(FALSE);
         }

         while(jbstrcpy(buf2,cfgbuf,100,&jbcpos))
         {
            if(!(tmpremnode=(struct RemNode *)osAllocCleared(sizeof(struct RemNode))))
            {
               *seconderr=READCONFIG_NO_MEM;
               osClose(cfgfh);
               return(FALSE);
            }

            jbAddNode(&LastAka->RemList,(struct jbNode *)tmpremnode);

            if(!Parse2DPat(buf2,&tmpremnode->NodePat))
            {
               sprintf(cfgerr,"Invalid node pattern \"%s\"",buf2);
               osClose(cfgfh);
               return(FALSE);
            }
         }
      }
      else if(stricmp(cfgword,"DOMAIN")==0)
      {
         if(!LastAka)
         {
            strcpy(cfgerr,"No previous AKA line");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(LastAka->Domain,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }
      }
      else if(stricmp(cfgword,"GROUPNAME")==0)
      {
         if(!(jbstrcpy(buf2,cfgbuf,2,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!CorrectFlags(buf2))
         {
            sprintf(cfgerr,"Invalid group \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(cfg->cfg_GroupNames[buf2[0]-'A'],cfgbuf,80,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"NODE")==0)
      {
         if(!(tmpnode=(struct ConfigNode *)osAllocCleared(sizeof(struct ConfigNode))))
         {
            *seconderr=READCONFIG_NO_MEM;
            osClose(cfgfh);
            return(FALSE);
         }

         jbNewList(&tmpnode->RemoteAFList);
         jbAddNode(&cfg->CNodeList,(struct jbNode *)tmpnode);
         LastCNode=tmpnode;

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(Parse4D(buf2,&LastCNode->Node)))
         {
            sprintf(cfgerr,"Invalid node number \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(buf2,cfgbuf,10,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         tmppacker=NULL;

         if(buf2[0]!=0)
         {
            for(tmppacker=(struct Packer *)cfg->PackerList.First;tmppacker;tmppacker=tmppacker->Next)
               if(stricmp(buf2,tmppacker->Name)==0) break;

            if(!tmppacker)
            {
               sprintf(cfgerr,"Unknown packer \"%s\"",buf2);
               osClose(cfgfh);
               return(FALSE);
            }
         }

         LastCNode->Packer=tmppacker;

         if(!(jbstrcpy(LastCNode->PacketPW,cfgbuf,9,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

			LastCNode->EchomailPri=PKTS_NORMAL;

         while(jbstrcpy(buf2,cfgbuf,200,&jbcpos))
         {
            if(stricmp(buf2,"NOTIFY")==0)
               LastCNode->Flags|=NODE_NOTIFY;

            else if(stricmp(buf2,"PASSIVE")==0)
               LastCNode->Flags|=NODE_PASSIVE;

            else if(stricmp(buf2,"NOSEENBY")==0)
               LastCNode->Flags|=NODE_NOSEENBY;

            else if(stricmp(buf2,"TINYSEENBY")==0)
               LastCNode->Flags|=NODE_TINYSEENBY;

            else if(stricmp(buf2,"FORWARDREQ")==0)
               LastCNode->Flags|=NODE_FORWARDREQ;

            else if(stricmp(buf2,"PACKNETMAIL")==0)
               LastCNode->Flags|=NODE_PACKNETMAIL;

            else if(stricmp(buf2,"SENDAREAFIX")==0)
               LastCNode->Flags|=NODE_SENDAREAFIX;

            else if(stricmp(buf2,"SENDTEXT")==0)
               LastCNode->Flags|=NODE_SENDTEXT;

            else if(stricmp(buf2,"AUTOADD")==0)
               LastCNode->Flags|=NODE_AUTOADD;

            else if(stricmp(buf2,"CRASH")==0)
               LastCNode->EchomailPri=PKTS_CRASH;

            else if(stricmp(buf2,"DIRECT")==0)
               LastCNode->EchomailPri=PKTS_DIRECT;

            else if(stricmp(buf2,"HOLD")==0)
               LastCNode->EchomailPri=PKTS_HOLD;

            else
            {
               sprintf(cfgerr,"Unknown switch \"%s\"",buf2);
               osClose(cfgfh);
               return(FALSE);
            }
         }
      }
      else if(stricmp(cfgword,"AREAFIXINFO")==0)
      {
         if(!LastCNode)
         {
            strcpy(cfgerr,"No previous NODE line");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(LastCNode->AreafixPW,cfgbuf,40,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(jbstrcpy(LastCNode->Groups,cfgbuf,70,&jbcpos))
         {
            if(!CorrectFlags(LastCNode->Groups))
            {
               sprintf(cfgerr,"Invalid groups \"%s\"",LastCNode->Groups);
               osClose(cfgfh);
               return(FALSE);
            }
         }

         if(jbstrcpy(LastCNode->ReadOnlyGroups,cfgbuf,70,&jbcpos))
         {
            if(!CorrectFlags(LastCNode->ReadOnlyGroups))
            {
               sprintf(cfgerr,"Invalid groups \"%s\"",LastCNode->ReadOnlyGroups);
               osClose(cfgfh);
               return(FALSE);
            }
         }

         if(jbstrcpy(LastCNode->AddGroups,cfgbuf,70,&jbcpos))
         {
            if(!CorrectFlags(LastCNode->AddGroups))
            {
               sprintf(cfgerr,"Invalid groups \"%s\"",LastCNode->AddGroups);
               osClose(cfgfh);
               return(FALSE);
            }
         }
      }
      else if(stricmp(cfgword,"DEFAULTGROUP")==0)
      {
         if(!LastCNode)
         {
            strcpy(cfgerr,"No previous NODE line");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(buf2,cfgbuf,2,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!CorrectFlags(buf2))
         {
            sprintf(cfgerr,"Invalid group \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         LastCNode->DefaultGroup=buf2[0];
      }
      else if(stricmp(cfgword,"AREA")==0 || stricmp(cfgword,"NETMAIL")==0)
      {
         if(!(tmparea=(struct Area *)osAllocCleared(sizeof(struct Area))))
         {
            *seconderr=READCONFIG_NO_MEM;
            osClose(cfgfh);
            return(FALSE);
         }

         jbNewList(&tmparea->TossNodes);
         jbNewList(&tmparea->BannedNodes);

         jbAddNode(&cfg->AreaList,(struct jbNode *)tmparea);
         LastArea=tmparea;

         if(!(jbstrcpy(LastArea->Tagname,cfgbuf,80,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(Parse4D(buf2,&tmp4d)))
         {
            sprintf(cfgerr,"Invalid node number \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         for(tmpaka=(struct Aka *)cfg->AkaList.First;tmpaka;tmpaka=tmpaka->Next)
            if(Compare4D(&tmp4d,&tmpaka->Node)==0) break;

         if(!tmpaka)
         {
            sprintf(cfgerr,"Unknown AKA \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         LastArea->Aka=tmpaka;

         if(jbstrcpy(buf2,cfgbuf,200,&jbcpos))
         {
            int c;

            for(c=0;AvailMessagebases[c].name;c++)
               if(stricmp(buf2,AvailMessagebases[c].name)==0) break;

            if(AvailMessagebases[c].name)
            {
               LastArea->Messagebase= &AvailMessagebases[c];
            }
            else
            {
               sprintf(cfgerr,"Unknown messagebase format \"%s\"",buf2);
               osClose(cfgfh);
               return(FALSE);
            }

            if(!(jbstrcpy(LastArea->Path,cfgbuf,80,&jbcpos)))
            {
               strcpy(cfgerr,"Missing argument");
               osClose(cfgfh);
               return(FALSE);
            }
         }

         if(stricmp(cfgword,"NETMAIL")==0)
         {
            if(!LastArea->Messagebase)
            {
               sprintf(cfgerr,"Netmail area cannot be pass-through");
               osClose(cfgfh);
               return(FALSE);
            }

            LastArea->Flags|=AREA_NETMAIL;
         }
         else if(stricmp(LastArea->Tagname,"BAD")==0)
         {
            if(LastArea->Path[0]==0)
            {
               sprintf(cfgerr,"BAD area cannot be pass-through");
               osClose(cfgfh);
               return(FALSE);
            }

            LastArea->Flags|=AREA_BAD;
         }
         else if(stricmp(LastArea->Tagname,"DEFAULT")==0 || strnicmp(LastArea->Tagname,"DEFAULT_",8)==0)
         {
            LastArea->Flags|=AREA_DEFAULT;
         }
      }
      else if(stricmp(cfgword,"UNCONFIRMED")==0)
      {
         if(!LastArea)
         {
            strcpy(cfgerr,"No previous AREA line");
            osClose(cfgfh);
            return(FALSE);
         }

         LastArea->Flags|=AREA_UNCONFIRMED;
      }
      else if(stricmp(cfgword,"MANDATORY")==0)
      {
         if(!LastArea)
         {
            strcpy(cfgerr,"No previous AREA line");
            osClose(cfgfh);
            return(FALSE);
         }

         LastArea->Flags|=AREA_MANDATORY;
      }
      else if(stricmp(cfgword,"DEFREADONLY")==0)
      {
         if(!LastArea)
         {
            strcpy(cfgerr,"No previous AREA line");
            osClose(cfgfh);
            return(FALSE);
         }

         LastArea->Flags|=AREA_DEFREADONLY;
      }
      else if(stricmp(cfgword,"IGNOREDUPES")==0)
      {
         if(!LastArea)
         {
            strcpy(cfgerr,"No previous AREA line");
            osClose(cfgfh);
            return(FALSE);
         }

         LastArea->Flags|=AREA_IGNOREDUPES;
      }
      else if(stricmp(cfgword,"IGNORESEENBY")==0)
      {
         if(!LastArea)
         {
            strcpy(cfgerr,"No previous AREA line");
            osClose(cfgfh);
            return(FALSE);
         }

         LastArea->Flags|=AREA_IGNORESEENBY;
      }
      else if(stricmp(cfgword,"EXPORT")==0)
      {
         if(!LastArea)
         {
            strcpy(cfgerr,"No previous AREA line");
            osClose(cfgfh);
            return(FALSE);
         }

         while(jbstrcpy(buf2,cfgbuf,100,&jbcpos))
         {
            flags=0;

            if(buf2[0]=='!')
            {
               flags=TOSSNODE_READONLY;
               strcpy(buf2,&buf2[1]);
            }

            if(buf2[0]=='@')
            {
               flags=TOSSNODE_WRITEONLY;
               strcpy(buf2,&buf2[1]);
            }

            if(buf2[0]=='%')
            {
               flags=TOSSNODE_FEED;
               strcpy(buf2,&buf2[1]);
            }

            if(!(Parse4D(buf2,&tmp4d)))
            {
               sprintf(cfgerr,"Invalid node number \"%s\"",buf2);
               osClose(cfgfh);
               return(FALSE);
            }

            for(tmpnode=(struct ConfigNode *)cfg->CNodeList.First;tmpnode;tmpnode=tmpnode->Next)
               if(Compare4D(&tmp4d,&tmpnode->Node)==0) break;

            if(!tmpnode)
            {
               sprintf(cfgerr,"Unconfigured node \"%s\"",buf2);
               osClose(cfgfh);
               return(FALSE);
            }

            if(!(tmptnode=(struct TossNode *)osAllocCleared(sizeof(struct TossNode))))
            {
               *seconderr=READCONFIG_NO_MEM;
               osClose(cfgfh);
               return(FALSE);
            }

            jbAddNode(&LastArea->TossNodes,(struct jbNode *)tmptnode);
            tmptnode->ConfigNode=tmpnode;
            tmptnode->Flags=flags;
         }
      }
      else if(stricmp(cfgword,"IMPORT")==0)
      {
         if(!LastArea)
         {
            strcpy(cfgerr,"No previous AREA line");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(LastArea->Flags & AREA_NETMAIL))
         {
            strcpy(cfgerr,"Not a netmail area");
            osClose(cfgfh);
            return(FALSE);
         }

         while(jbstrcpy(buf2,cfgbuf,100,&jbcpos))
         {
            if(!(tmpinode=(struct ImportNode *)osAllocCleared(sizeof(struct ImportNode))))
            {
               *seconderr=READCONFIG_NO_MEM;
               osClose(cfgfh);
               return(FALSE);
            }

            jbAddNode(&LastArea->TossNodes,(struct jbNode *)tmpinode);

            if(!(Parse4D(buf2,&tmpinode->Node)))
            {
               sprintf(cfgerr,"Invalid node number \"%s\"",buf2);
               osClose(cfgfh);
               return(FALSE);
            }
         }
      }
      else if(stricmp(cfgword,"BANNED")==0)
      {
         if(!LastArea)
         {
            strcpy(cfgerr,"No previous AREA line");
            osClose(cfgfh);
            return(FALSE);
         }

         while(jbstrcpy(buf2,cfgbuf,100,&jbcpos))
         {
            if(!(Parse4D(buf2,&tmp4d)))
            {
               sprintf(cfgerr,"Invalid node number \"%s\"",buf2);
               osClose(cfgfh);
               return(FALSE);
            }

            for(tmpnode=(struct ConfigNode *)cfg->CNodeList.First;tmpnode;tmpnode=tmpnode->Next)
               if(Compare4D(&tmp4d,&tmpnode->Node)==0) break;

            if(!tmpnode)
            {
               sprintf(cfgerr,"Unconfigured node \"%s\"",buf2);
               osClose(cfgfh);
               return(FALSE);
            }

            if(!(tmpbnode=(struct BannedNode *)osAllocCleared(sizeof(struct BannedNode))))
            {
               *seconderr=READCONFIG_NO_MEM;
               osClose(cfgfh);
               return(FALSE);
            }

            jbAddNode(&LastArea->BannedNodes,(struct jbNode *)tmpbnode);
            tmpbnode->ConfigNode=tmpnode;
         }
      }
      else if(stricmp(cfgword,"DESCRIPTION")==0)
      {
         if(!LastArea)
         {
            strcpy(cfgerr,"No previous AREA line");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(LastArea->Description,cfgbuf,80,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"GROUP")==0)
      {
         if(!LastArea)
         {
            strcpy(cfgerr,"No previous AREA line");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(buf2,cfgbuf,30,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!CorrectFlags(buf2))
         {
            sprintf(cfgerr,"Invalid group \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         LastArea->Group=buf2[0];
      }
      else if(stricmp(cfgword,"KEEPNUM")==0)
      {
         if(!LastArea)
         {
            strcpy(cfgerr,"No previous AREA line");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(buf2,cfgbuf,30,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         LastArea->KeepNum=atol(buf2);
      }
      else if(stricmp(cfgword,"KEEPDAYS")==0)
      {
         if(!LastArea)
         {
            strcpy(cfgerr,"No previous AREA line");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(buf2,cfgbuf,30,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         LastArea->KeepDays=atol(buf2);
      }
      else if(stricmp(cfgword,"AREAFIXNAME")==0)
      {
         if(!(tmpareafixname=(struct AreaFixName *)osAllocCleared(sizeof(struct AreaFixName))))
         {
            *seconderr=READCONFIG_NO_MEM;
            osClose(cfgfh);
            return(FALSE);
         }

         jbAddNode(&cfg->AreaFixList,(struct jbNode *)tmpareafixname);

         if(!(jbstrcpy(tmpareafixname->Name,cfgbuf,36,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"FILEATTACH")==0)
      {
         while(jbstrcpy(buf2,cfgbuf,100,&jbcpos))
         {
            if(!(tmppatternnode=(struct PatternNode *)osAllocCleared(sizeof(struct PatternNode))))
            {
               *seconderr=READCONFIG_NO_MEM;
               osClose(cfgfh);
               return(FALSE);
            }

            if(!(Parse4DPat(buf2,&tmppatternnode->Pattern)))
            {
               sprintf(cfgerr,"Invalid node patternt \"%s\"",buf2);
               osClose(cfgfh);
               return(FALSE);
            }

            jbAddNode(&cfg->FileAttachList,(struct jbNode *)tmppatternnode);
         }
      }
      else if(stricmp(cfgword,"BOUNCE")==0)
      {
         while(jbstrcpy(buf2,cfgbuf,100,&jbcpos))
         {
            if(!(tmppatternnode=(struct PatternNode *)osAllocCleared(sizeof(struct PatternNode))))
            {
               *seconderr=READCONFIG_NO_MEM;
               osClose(cfgfh);
               return(FALSE);
            }

            if(!(Parse4DPat(buf2,&tmppatternnode->Pattern)))
            {
               sprintf(cfgerr,"Invalid node patternt \"%s\"",buf2);
               osClose(cfgfh);
               return(FALSE);
            }

            jbAddNode(&cfg->BounceList,(struct jbNode *)tmppatternnode);
         }
      }
      else if(stricmp(cfgword,"INBOUND")==0)
      {
         if(!(jbstrcpy(cfg->cfg_Inbound,cfgbuf,100,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"OUTBOUND")==0)
      {
         if(!(jbstrcpy(cfg->cfg_Outbound,cfgbuf,100,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"SYSOP")==0)
      {
         if(!(jbstrcpy(cfg->cfg_Sysop,cfgbuf,35,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }
      }
      else if(stricmp(cfgword,"NODELIST")==0)
      {
         int i;

         if(!(jbstrcpy(cfg->cfg_Nodelist,cfgbuf,100,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         cfg->cfg_NodelistType=NULL;

         if(!(jbstrcpy(buf2,cfgbuf,40,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         for(i=0;AvailNodelists[i].name;i++)
            if(stricmp(buf2,AvailNodelists[i].name)==0) break;

         if(!AvailNodelists[i].name)
         {
            sprintf(cfgerr,"Unsupported nodelist type \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         cfg->cfg_NodelistType= &AvailNodelists[i];
      }
      else if(stricmp(cfgword,"AREAFIXHELP")==0)
      {
         if(!(jbstrcpy(cfg->cfg_AreaFixHelp,cfgbuf,100,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"LOGFILE")==0)
      {
         if(!(jbstrcpy(cfg->cfg_LogFile,cfgbuf,100,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"LOGLEVEL")==0)
      {
         if(!(jbstrcpy(buf2,cfgbuf,100,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(atoi(buf2)<1 || atoi(buf2)>6)
         {
            strcpy(cfgerr,"Loglevel out of range");
            osClose(cfgfh);
            return(FALSE);
         }

         cfg->cfg_LogLevel=atoi(buf2);
      }
      else if(stricmp(cfgword,"STATSFILE")==0)
      {
         if(!(jbstrcpy(cfg->cfg_StatsFile,cfgbuf,100,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }
      }
      else if(stricmp(cfgword,"DUPEFILE")==0)
      {
         if(!(jbstrcpy(cfg->cfg_DupeFile,cfgbuf,100,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         cfg->cfg_DupeSize=atoi(buf2)*1024;
      }
      else if(stricmp(cfgword,"DUPEMODE")==0)
      {
         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(stricmp(buf2,"IGNORE")==0)
            cfg->cfg_DupeMode=DUPE_IGNORE;

         else if(stricmp(buf2,"KILL")==0)
            cfg->cfg_DupeMode=DUPE_KILL;

         else if(stricmp(buf2,"BAD")==0)
            cfg->cfg_DupeMode=DUPE_BAD;

         else
         {
            sprintf(cfgerr,"Unknown dupemode \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }
      }
      else if(stricmp(cfgword,"LOOPMODE")==0)
      {
         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(stricmp(buf2,"IGNORE")==0)
            cfg->cfg_LoopMode=LOOP_IGNORE;

         else if(stricmp(buf2,"LOG")==0)
            cfg->cfg_LoopMode=LOOP_LOG;

         else if(stricmp(buf2,"LOG+BAD")==0)
            cfg->cfg_LoopMode=LOOP_LOGBAD;

         else
         {
            sprintf(cfgerr,"Unknown loopmode \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }
      }
      else if(stricmp(cfgword,"TEMPDIR")==0)
      {
         if(!(jbstrcpy(cfg->cfg_TempDir,cfgbuf,100,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"CREATEPKTDIR")==0)
      {
         if(!(jbstrcpy(cfg->cfg_PacketCreate,cfgbuf,100,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"PACKETDIR")==0)
      {
         if(!(jbstrcpy(cfg->cfg_PacketDir,cfgbuf,100,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"PACKER")==0)
      {
         if(!(tmppacker=(struct Packer *)osAllocCleared(sizeof(struct Packer))))
         {
            *seconderr=READCONFIG_NO_MEM;
            osClose(cfgfh);
            return(FALSE);
         }

         jbAddNode(&cfg->PackerList,(struct jbNode *)tmppacker);
         LastPacker=tmppacker;

         if(!(jbstrcpy(LastPacker->Name,cfgbuf,10,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }


         if(!(jbstrcpy(LastPacker->Packer,cfgbuf,80,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }


         if(!(jbstrcpy(LastPacker->Unpacker,cfgbuf,80,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(LastPacker->Recog,cfgbuf,80,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"ROUTE")==0)
      {
         if(!(tmproute=(struct Route *)osAllocCleared(sizeof(struct Route))))
         {
            *seconderr=READCONFIG_NO_MEM;
            osClose(cfgfh);
            return(FALSE);
         }

         jbAddNode(&cfg->RouteList,(struct jbNode *)tmproute);

         /* Pattern */

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(Parse4DPat(buf2,&tmproute->Pattern)))
         {
            sprintf(cfgerr,"Invalid node pattern \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         /* Dest */

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(Parse4DDestPat(buf2,&tmproute->DestPat)))
         {
            sprintf(cfgerr,"Invalid node pattern \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         /* Aka */

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(Parse4D(buf2,&tmp4d)))
         {
            sprintf(cfgerr,"Invalid node number \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         for(tmpaka=(struct Aka *)cfg->AkaList.First;tmpaka;tmpaka=tmpaka->Next)
            if(Compare4D(&tmp4d,&tmpaka->Node)==0) break;

         if(!tmpaka)
         {
            sprintf(cfgerr,"Unconfigured AKA \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         tmproute->Aka=tmpaka;
      }
      else if(stricmp(cfgword,"CHANGE")==0)
      {
         if(!(tmpchange=(struct Change *)osAllocCleared(sizeof(struct Change))))
         {
            *seconderr=READCONFIG_NO_MEM;
            osClose(cfgfh);
            return(FALSE);
         }

         jbAddNode(&cfg->ChangeList,(struct jbNode *)tmpchange);

         /* Type pattern */

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(buf2[0]=='*')
         {
            tmpchange->ChangeNormal=TRUE;
            tmpchange->ChangeCrash=TRUE;
            tmpchange->ChangeHold=TRUE;
            tmpchange->ChangeDirect=TRUE;
         }
         else
         {
            c=0;
            while(buf2[c]!=0)
            {
               d=c;
               while(buf2[d]!=',' && buf2[d]!=0) d++;
               if(buf2[d]==',')
               {
                  buf2[d]=0;
                  d++;
               }

               if(stricmp(&buf2[c],"NORMAL")==0)
                  tmpchange->ChangeNormal=TRUE;

               else if(stricmp(&buf2[c],"CRASH")==0)
                  tmpchange->ChangeCrash=TRUE;

               else if(stricmp(&buf2[c],"HOLD")==0)
                  tmpchange->ChangeHold=TRUE;

               else if(stricmp(&buf2[c],"DIRECT")==0)
                  tmpchange->ChangeDirect=TRUE;

               else
               {
                  sprintf(cfgerr,"Unknown mail flavour \"%s\"",buf2);
                  osClose(cfgfh);
                  return(FALSE);
               }

               c=d;
            }
         }

         /* Node pattern */

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(Parse4DPat(buf2,&tmpchange->Pattern)))
         {
            sprintf(cfgerr,"Invalid node pattern \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         /* New type */

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(stricmp(buf2,"NORMAL")==0)
            tmpchange->DestPri=PKTS_NORMAL;

         else if(stricmp(buf2,"CRASH")==0)
            tmpchange->DestPri=PKTS_CRASH;

         else if(stricmp(buf2,"HOLD")==0)
            tmpchange->DestPri=PKTS_HOLD;

         else if(stricmp(buf2,"DIRECT")==0)
            tmpchange->DestPri=PKTS_DIRECT;

         else
         {
            sprintf(cfgerr,"Unknown mail flavour \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }
      }
      else if(stricmp(cfgword,"REMAP")==0)
      {
         if(!(tmpremap=(struct Remap *)osAllocCleared(sizeof(struct Remap))))
         {
            *seconderr=READCONFIG_NO_MEM;
            osClose(cfgfh);
            return(FALSE);
         }

         jbAddNode(&cfg->RemapList,(struct jbNode *)tmpremap);

         /* Type pattern */

         if(!(jbstrcpy(tmpremap->Pattern,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!osCheckPattern(tmpremap->Pattern))
         {
            sprintf(cfgerr,"Invalid pattern \"%s\"",tmpremap->Pattern);
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(tmpremap->NewTo,cfgbuf,36,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(Parse4DDestPat(buf2,&tmpremap->DestPat)))
         {
            sprintf(cfgerr,"Invalid destination pattern \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }
      }
      else if(stricmp(cfgword,"REMAPNODE")==0)
      {
         if(!(tmpremapnode=(struct RemapNode *)osAllocCleared(sizeof(struct RemapNode))))
         {
            *seconderr=READCONFIG_NO_MEM;
            osClose(cfgfh);
            return(FALSE);
         }

         jbAddNode(&cfg->RemapNodeList,(struct jbNode *)tmpremapnode);

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(Parse4DPat(buf2,&tmpremapnode->NodePat)))
         {
            sprintf(cfgerr,"Invalid node pattern \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(Parse4DDestPat(buf2,&tmpremapnode->DestPat)))
         {
            sprintf(cfgerr,"Invalid destination pattern \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }
      }
      else if(stricmp(cfgword,"ROBOTNAME")==0)
      {
         if(!(tmprobot=(struct Robot *)osAllocCleared(sizeof(struct Robot))))
         {
            *seconderr=READCONFIG_NO_MEM;
            osClose(cfgfh);
            return(FALSE);
         }

         jbAddNode(&cfg->RobotList,(struct jbNode *)tmprobot);

         if(!(jbstrcpy(tmprobot->Pattern,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!osCheckPattern(tmprobot->Pattern))
         {
            sprintf(cfgerr,"Invalid pattern \"%s\"",tmprobot->Pattern);
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(tmprobot->Command,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"AREALIST")==0)
      {
         if(!(tmparealist=(struct Arealist *)osAllocCleared(sizeof(struct Arealist))))
         {
            *seconderr=READCONFIG_NO_MEM;
            osClose(cfgfh);
            return(FALSE);
         }

         jbAddNode(&cfg->ArealistList,(struct jbNode *)tmparealist);

         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(Parse4D(buf2,&tmp4d)))
         {
            sprintf(cfgerr,"Invalid node number \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         for(tmpnode=(struct ConfigNode *)cfg->CNodeList.First;tmpnode;tmpnode=tmpnode->Next)
            if(Compare4D(&tmp4d,&tmpnode->Node)==0) break;

         if(!tmpnode)
         {
            sprintf(cfgerr,"Unconfigured node \"%s\"",buf2);
            osClose(cfgfh);
            return(FALSE);
         }

         tmparealist->Node=tmpnode;

         if(!(jbstrcpy(tmparealist->AreaFile,cfgbuf,80,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         while(jbstrcpy(buf2,cfgbuf,200,&jbcpos))
         {
            if(stricmp(buf2,"FORWARD")==0)
            {
               tmparealist->Flags|=AREALIST_FORWARD;
            }
            else if(stricmp(buf2,"DESC")==0)
            {
               tmparealist->Flags|=AREALIST_DESC;
            }
            else if(stricmp(buf2,"GROUP")==0)
            {
               if(!jbstrcpy(buf2,cfgbuf,200,&jbcpos))
               {
                  strcpy(cfgerr,"Missing argument");
                  osClose(cfgfh);
                  return(FALSE);
               }

               if(!CorrectFlags(buf2))
               {
                  sprintf(cfgerr,"Invalid group \"%s\"",buf2);
                  osClose(cfgfh);
                  return(FALSE);
               }

               tmparealist->Group=buf2[0];
            }
            else
            {
               sprintf(cfgerr,"Unknown switch \"%s\"",buf2);
               osClose(cfgfh);
               return(FALSE);
            }
         }
      }
      else if(stricmp(cfgword,"REMOTEAF")==0)
      {
         if(!LastCNode)
         {
            strcpy(cfgerr,"No previous NODE line");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(LastCNode->RemoteAFName,cfgbuf,36,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(LastCNode->RemoteAFPw,cfgbuf,72,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"REMOTESYSOP")==0)
      {
         if(!LastCNode)
         {
            strcpy(cfgerr,"No previous NODE line");
            osClose(cfgfh);
            return(FALSE);
         }

         if(!(jbstrcpy(LastCNode->SysopName,cfgbuf,36,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

      }
      else if(stricmp(cfgword,"STRIPRE")==0)
      {
         cfg->cfg_Flags|=CFG_STRIPRE;
      }
      else if(stricmp(cfgword,"FORCEINTL")==0)
      {
         cfg->cfg_Flags|=CFG_FORCEINTL;
      }
      else if(stricmp(cfgword,"BOUNCEHEADERONLY")==0)
      {
         cfg->cfg_Flags|=CFG_BOUNCEHEADERONLY;
      }
      else if(stricmp(cfgword,"REMOVEWHENFEED")==0)
      {
         cfg->cfg_Flags|=CFG_REMOVEWHENFEED;
      }
      else if(stricmp(cfgword,"INCLUDEFORWARD")==0)
      {
         cfg->cfg_Flags|=CFG_INCLUDEFORWARD;
      }
      else if(stricmp(cfgword,"NOMAXOUTBOUNDZONE")==0)
      {
         cfg->cfg_Flags|=CFG_NOMAXOUTBOUNDZONE;
      }
      else if(stricmp(cfgword,"ALLOWKILLSENT")==0)
      {
         cfg->cfg_Flags|=CFG_ALLOWKILLSENT;
      }
      else if(stricmp(cfgword,"NOROUTE")==0)
      {
         cfg->cfg_Flags|=CFG_NOROUTE;
      }
      else if(stricmp(cfgword,"ANSWERRECEIPT")==0)
      {
         cfg->cfg_Flags|=CFG_ANSWERRECEIPT;
      }
      else if(stricmp(cfgword,"ANSWERAUDIT")==0)
      {
         cfg->cfg_Flags|=CFG_ANSWERAUDIT;
      }
      else if(stricmp(cfgword,"CHECKSEENBY")==0)
      {
         cfg->cfg_Flags|=CFG_CHECKSEENBY;
      }
      else if(stricmp(cfgword,"CHECKPKTDEST")==0)
      {
         cfg->cfg_Flags|=CFG_CHECKPKTDEST;
      }
      else if(stricmp(cfgword,"PATH3D")==0)
      {
         cfg->cfg_Flags|=CFG_PATH3D;
      }
      else if(stricmp(cfgword,"IMPORTEMPTYNETMAIL")==0)
      {
         cfg->cfg_Flags|=CFG_IMPORTEMPTYNETMAIL;
      }
      else if(stricmp(cfgword,"IMPORTAREAFIX")==0)
      {
         cfg->cfg_Flags|=CFG_IMPORTAREAFIX;
      }
      else if(stricmp(cfgword,"AREAFIXREMOVE")==0)
      {
         cfg->cfg_Flags|=CFG_AREAFIXREMOVE;
      }
      else if(stricmp(cfgword,"NODIRECTATTACH")==0)
      {
         cfg->cfg_Flags|=CFG_NODIRECTATTACH;
      }
      else if(stricmp(cfgword,"BOUNCEPOINTS")==0)
      {
         cfg->cfg_Flags|=CFG_BOUNCEPOINTS;
      }
      else if(stricmp(cfgword,"IMPORTSEENBY")==0)
      {
         cfg->cfg_Flags|=CFG_IMPORTSEENBY;
      }
      else if(stricmp(cfgword,"WEEKDAYNAMING")==0)
      {
         cfg->cfg_Flags|=CFG_WEEKDAYNAMING;
      }
      else if(stricmp(cfgword,"ADDTID")==0)
      {
         cfg->cfg_Flags|=CFG_ADDTID;
      }
      else if(stricmp(cfgword,"ALLOWRESCAN")==0)
      {
         cfg->cfg_Flags|=CFG_ALLOWRESCAN;
      }
      else if(stricmp(cfgword,"FORWARDPASSTHRU")==0)
      {
         cfg->cfg_Flags|=CFG_FORWARDPASSTHRU;
      }
      else if(stricmp(cfgword,"MAXPKTSIZE")==0)
      {
         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         cfg->cfg_MaxPktSize=atoi(buf2)*1024;
      }
      else if(stricmp(cfgword,"MAXBUNDLESIZE")==0)
      {
         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         cfg->cfg_MaxBundleSize=atoi(buf2)*1024;
      }
      else if(stricmp(cfgword,"DEFAULTZONE")==0)
      {
         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         cfg->cfg_DefaultZone=atoi(buf2);
      }
      else if(stricmp(cfgword,"AREAFIXMAXLINES")==0)
      {
         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         cfg->cfg_AreaFixMaxLines=atoi(buf2);
      }

      /*************************** MSG ******************************/
#ifdef MSGBASE_MSG
      else if(stricmp(cfgword,"MSG_HIGHWATER")==0)
      {
         cfg->cfg_msg_Flags|=CFG_MSG_HIGHWATER;
      }
      else if(stricmp(cfgword,"MSG_WRITEBACK")==0)
      {
         cfg->cfg_msg_Flags|=CFG_MSG_WRITEBACK;
      }
 #endif
      /*************************** JAM ******************************/
#ifdef MSGBASE_JAM
      else if(stricmp(cfgword,"JAM_HIGHWATER")==0)
      {
         cfg->cfg_jam_Flags|=CFG_JAM_HIGHWATER;
      }
      else if(stricmp(cfgword,"JAM_LINK")==0)
      {
         cfg->cfg_jam_Flags|=CFG_JAM_LINK;
      }
      else if(stricmp(cfgword,"JAM_QUICKLINK")==0)
      {
         cfg->cfg_jam_Flags|=CFG_JAM_QUICKLINK;
      }
      else if(stricmp(cfgword,"JAM_MAXOPEN")==0)
      {
         if(!(jbstrcpy(buf2,cfgbuf,200,&jbcpos)))
         {
            strcpy(cfgerr,"Missing argument");
            osClose(cfgfh);
            return(FALSE);
         }

         cfg->cfg_jam_MaxOpen=atoi(buf2);
      }
#endif
      /*************************** Unknown *****************************/

      else if(cfgword[0]!=0 && cfgword[0]!=';')
      {
         sprintf(cfgerr,"Unknown keyword \"%s\"",cfgword);
         osClose(cfgfh);
         return(FALSE);
      }
   }

   osClose(cfgfh);

   return(TRUE);
}

bool CheckConfig(struct Config *cfg,uchar *cfgerr)
{
   struct Area *a1,*a2;
   struct PatternNode *patternnode;
   struct Route *route;
   struct Change *change;
   struct RemapNode *remapnode;
   uchar buf[50];

   if(cfg->cfg_TempDir[0]==0)      strcpy(cfg->cfg_TempDir,cfg->cfg_Inbound);
   if(cfg->cfg_PacketCreate[0]==0) strcpy(cfg->cfg_PacketCreate,cfg->cfg_Outbound);
   if(cfg->cfg_PacketDir[0]==0)    strcpy(cfg->cfg_PacketDir,cfg->cfg_Outbound);

   if(!cfg->AkaList.First)
   {
      sprintf(cfgerr,"No AKAs configured");
      return(FALSE);
   }

   if(!cfg->cfg_NodelistType)
   {
      /* Check if any pattern needs a nodelist */

      for(patternnode=(struct PatternNode *)cfg->FileAttachList.First;patternnode;patternnode=patternnode->Next)
         if(!Check4DPatNodelist(&patternnode->Pattern))
         {
            Print4DPat(&patternnode->Pattern,buf);
            sprintf(cfgerr,"Nodelist needed for pattern \"%s\"",buf);
            return(FALSE);
         }

      for(patternnode=(struct PatternNode *)cfg->BounceList.First;patternnode;patternnode=patternnode->Next)
         if(!Check4DPatNodelist(&patternnode->Pattern))
         {
            Print4DPat(&patternnode->Pattern,buf);
         sprintf(cfgerr,"Nodelist needed for pattern \"%s\"",buf);
         return(FALSE);
      }

      for(route=(struct Route *)cfg->RouteList.First;route;route=route->Next)
      {
         if(!Check4DPatNodelist(&route->Pattern))
         {
            Print4DPat(&route->Pattern,buf);
            sprintf(cfgerr,"Nodelist needed for pattern \"%s\"",buf);
            return(FALSE);
         }

         if(!Check4DPatNodelist(&route->DestPat))
         {
            Print4DDestPat(&route->DestPat,buf);
            sprintf(cfgerr,"Nodelist needed for pattern \"%s\"",buf);
            return(FALSE);
         }
      }

      for(change=(struct Change *)cfg->ChangeList.First;change;change=change->Next)
         if(!Check4DPatNodelist(&change->Pattern))
         {
            Print4DPat(&change->Pattern,buf);
            sprintf(cfgerr,"Nodelist needed for pattern \"%s\"",buf);
            return(FALSE);
         }

      for(remapnode=(struct RemapNode *)cfg->RemapNodeList.First;remapnode;remapnode=remapnode->Next)
      {
         if(!Check4DPatNodelist(&remapnode->NodePat))
         {
            Print4DPat(&remapnode->NodePat,buf);
            sprintf(cfgerr,"Nodelist needed for pattern \"%s\"",buf);
            return(FALSE);
         }

         if(!Check4DPatNodelist(&remapnode->DestPat))
         {
            Print4DDestPat(&remapnode->DestPat,buf);
            sprintf(cfgerr,"Nodelist needed for pattern \"%s\"",buf);
            return(FALSE);
         }
      }
   }

   /* Check for areas with same path */

   for(a1=(struct Area *)cfg->AreaList.First;a1;a1=a1->Next)
      for(a2=a1->Next;a2;a2=a2->Next)
         if(stricmp(a1->Path,a2->Path)==0 && a1->Messagebase && a2->Messagebase)
         {
            sprintf(cfgerr,"The areas %s and %s both have the same path",a1->Tagname,a2->Tagname);
            return(FALSE);
         }

   return(TRUE);
}

void InitConfig(struct Config *cfg)
{
   memset(cfg,0,sizeof(struct Config));

   strcpy(cfg->cfg_Sysop,"Sysop");

   cfg->cfg_LogLevel=3;
   cfg->cfg_DupeSize=200;

   cfg->cfg_DefaultZone=2;

   jbNewList(&cfg->AkaList);
   jbNewList(&cfg->AreaList);
   jbNewList(&cfg->CNodeList);
   jbNewList(&cfg->PackerList);
   jbNewList(&cfg->RouteList);
   jbNewList(&cfg->FileAttachList);
   jbNewList(&cfg->BounceList);
   jbNewList(&cfg->ChangeList);
   jbNewList(&cfg->RemapList);
   jbNewList(&cfg->RemapNodeList);
   jbNewList(&cfg->RobotList);
   jbNewList(&cfg->AreaFixList);
   jbNewList(&cfg->ArealistList);
}

void WriteSafely(osFile fh,uchar *str)
{
   uchar buf[300];
   ushort c,d;

   d=0;

   for(c=0;str[c];c++)
   {
      if(str[c]=='"' || str[c]=='\\')
         buf[d++]='\\';

      buf[d++]=str[c];
   }

   buf[d]=0;

   osFPrintf(fh,"\"%s\"",buf);
}

void WriteNode4D(osFile fh,struct Node4D *n4d)
{
   osFPrintf(fh,"%lu:%lu/%lu.%lu",n4d->Zone,n4d->Net,n4d->Node,n4d->Point);
}

uchar *nodekeywords[]={"DEFAULTGROUP","REMOTESYSOP","REMOTEAF",
                       "AREAFIXINFO","NODE",NULL};

uchar *areakeywords[]={"IGNORESEENBY","IGNOREDUPES","DEFREADONLY",
                       "MANDATORY","UNCONFIRMED","KEEPNUM","KEEPDAYS",
                       "GROUP","DESCRIPTION","BANNED","EXPORT","IMPORT",
                       "AREA","NETMAIL",NULL};

void WriteNode(struct ConfigNode *tmpnode,osFile osfh)
{
   osFPrintf(osfh,"NODE ");
   WriteNode4D(osfh,&tmpnode->Node);
   osFPrintf(osfh," ");

   if(tmpnode->Packer)
      WriteSafely(osfh,tmpnode->Packer->Name);

   else
      WriteSafely(osfh,"");

   osFPrintf(osfh," ");
   WriteSafely(osfh,tmpnode->PacketPW);

   if(tmpnode->Flags & NODE_PASSIVE)
      osFPrintf(osfh," PASSIVE");

   if(tmpnode->Flags & NODE_NOTIFY)
      osFPrintf(osfh," NOTIFY");

   if(tmpnode->Flags & NODE_NOSEENBY)
      osFPrintf(osfh," NOSEENBY");

   if(tmpnode->Flags & NODE_TINYSEENBY)
      osFPrintf(osfh," TINYSEENBY");

   if(tmpnode->Flags & NODE_FORWARDREQ)
      osFPrintf(osfh," FORWARDREQ");

   if(tmpnode->Flags & NODE_PACKNETMAIL)
      osFPrintf(osfh," PACKNETMAIL");

   if(tmpnode->Flags & NODE_SENDAREAFIX)
      osFPrintf(osfh," SENDAREAFIX");

   if(tmpnode->Flags & NODE_SENDTEXT)
      osFPrintf(osfh," SENDTEXT");

   if(tmpnode->Flags & NODE_AUTOADD)
      osFPrintf(osfh," AUTOADD");

   if(tmpnode->EchomailPri == PKTS_CRASH)
      osFPrintf(osfh," CRASH");

   if(tmpnode->EchomailPri == PKTS_DIRECT)
      osFPrintf(osfh," DIRECT");

   if(tmpnode->EchomailPri == PKTS_HOLD)
      osFPrintf(osfh," HOLD");

   osFPrintf(osfh,"\n");

   if(tmpnode->AreafixPW[0] || tmpnode->Groups[0] || tmpnode->ReadOnlyGroups[0] || tmpnode->AddGroups[0])
   {
      osFPrintf(osfh,"AREAFIXINFO ");
      WriteSafely(osfh,tmpnode->AreafixPW);
      osFPrintf(osfh," ");

      WriteSafely(osfh,tmpnode->Groups);
      osFPrintf(osfh," ");

      WriteSafely(osfh,tmpnode->ReadOnlyGroups);
      osFPrintf(osfh," ");

      WriteSafely(osfh,tmpnode->AddGroups);
      osFPrintf(osfh," ");

      osFPrintf(osfh,"\n");
   }

   if(tmpnode->RemoteAFName[0])
   {
      osFPrintf(osfh,"REMOTEAF ");
      WriteSafely(osfh,tmpnode->RemoteAFName);
      osFPrintf(osfh," ");
      WriteSafely(osfh,tmpnode->RemoteAFPw);
      osFPrintf(osfh,"\n");
   }

   if(tmpnode->SysopName[0])
   {
      osFPrintf(osfh,"REMOTESYSOP ");
      WriteSafely(osfh,tmpnode->SysopName);
      osFPrintf(osfh,"\n");
   }

   if(tmpnode->DefaultGroup)
      osFPrintf(osfh,"DEFAULTGROUP %lc\n",tmpnode->DefaultGroup);
}

void WriteArea(struct Area *tmparea,osFile osfh)
{
   struct ImportNode *tmpinode;
   struct TossNode   *tmptnode;
   struct BannedNode *tmpbnode;
   ulong c;

   if(tmparea->Flags & AREA_NETMAIL)
      osFPrintf(osfh,"NETMAIL ");

   else
      osFPrintf(osfh,"AREA ");

   WriteSafely(osfh,tmparea->Tagname);
   osFPrintf(osfh," ");
   WriteNode4D(osfh,&tmparea->Aka->Node);

   if(tmparea->Messagebase)
   {
      osFPrintf(osfh," %s ",tmparea->Messagebase->name);
      WriteSafely(osfh,tmparea->Path);
      osFPrintf(osfh,"\n");
   }
	else
	{
      osFPrintf(osfh,"\n");
	}
	      
   if(tmparea->Flags & AREA_NETMAIL)
   {
      c=0;

      for(tmpinode=(struct ImportNode *)tmparea->TossNodes.First;tmpinode;tmpinode=tmpinode->Next)
      {
         if(c%10==0)
         {
            if(c!=0) osFPrintf(osfh,"\n");
            osFPrintf(osfh,"IMPORT");
         }

         osFPrintf(osfh," ");
         WriteNode4D(osfh,&tmpinode->Node);
         c++;
      }
      if(c!=0) osFPrintf(osfh,"\n");
   }
   else
      {
      c=0;

      for(tmptnode=(struct TossNode *)tmparea->TossNodes.First;tmptnode;tmptnode=tmptnode->Next)
      {
         if(c%10==0)
         {
            if(c!=0) osFPrintf(osfh,"\n");
            osFPrintf(osfh,"EXPORT");
         }

         osFPrintf(osfh," ");

         if(tmptnode->Flags & TOSSNODE_READONLY)
            osFPrintf(osfh,"!");

         if(tmptnode->Flags & TOSSNODE_WRITEONLY)
            osFPrintf(osfh,"@");

         if(tmptnode->Flags & TOSSNODE_FEED)
            osFPrintf(osfh,"%%");

         WriteNode4D(osfh,&tmptnode->ConfigNode->Node);
         c++;
      }
      if(c!=0) osFPrintf(osfh,"\n");
   }

   c=0;
   for(tmpbnode=(struct BannedNode *)tmparea->BannedNodes.First;tmpbnode;tmpbnode=tmpbnode->Next)
   {
      if(c%10==0)
      {
         if(c!=0) osFPrintf(osfh,"\n");
         osFPrintf(osfh,"BANNED");
      }

      osFPrintf(osfh," ");
      WriteNode4D(osfh,&tmpbnode->ConfigNode->Node);
      c++;
   }
   if(c!=0) osFPrintf(osfh,"\n");

   if(tmparea->Description[0]!=0)
   {
      osFPrintf(osfh,"DESCRIPTION ");
      WriteSafely(osfh,tmparea->Description);
      osFPrintf(osfh,"\n");
   }

   if(tmparea->Group)
      osFPrintf(osfh,"GROUP %lc\n",tmparea->Group);

   if(tmparea->KeepNum)
      osFPrintf(osfh,"KEEPNUM %lu\n",tmparea->KeepNum);

   if(tmparea->KeepDays)
      osFPrintf(osfh,"KEEPDAYS %lu\n",tmparea->KeepDays);

   if(tmparea->Flags & AREA_UNCONFIRMED)
      osFPrintf(osfh,"UNCONFIRMED\n");

   if(tmparea->Flags & AREA_MANDATORY)
      osFPrintf(osfh,"MANDATORY\n");

   if(tmparea->Flags & AREA_DEFREADONLY)
      osFPrintf(osfh,"DEFREADONLY\n");

   if(tmparea->Flags & AREA_IGNOREDUPES)
      osFPrintf(osfh,"IGNOREDUPES\n");

   if(tmparea->Flags & AREA_IGNORESEENBY)
      osFPrintf(osfh,"IGNORESEENBY\n");
}

bool UpdateConfig(struct Config *cfg,uchar *cfgerr)
{
   uchar cfgtemp[110],cfgbak[110];
   uchar cfgword[30],buf[100];
   osFile oldfh,newfh;
   bool skiparea,skipnode,dontwrite;
   struct ConfigNode    *cnode;
   struct Area          *area;
   struct Node4D n4d;
   ulong jbcpos,c;

   strcpy(cfgtemp,cfg->filename);
   strcat(cfgtemp,".tmp");

   strcpy(cfgbak,cfg->filename);
   strcat(cfgbak,".bak");

   if(!(oldfh=osOpen(cfg->filename,MODE_OLDFILE)))
   {
      sprintf(cfgerr,"Unable to write to read file %s\n",cfgtemp);
      return(FALSE);
   }

   if(!(newfh=osOpen(cfgtemp,MODE_NEWFILE)))
   {
      sprintf(cfgerr,"Unable to write to config file %s\n",cfgtemp);
      osClose(oldfh);
      return(FALSE);
   }

   skiparea=FALSE;
   skipnode=FALSE;

   while(osFGets(oldfh,cfgbuf,4000))
   {
      jbcpos=0;
      jbstrcpy(cfgword,cfgbuf,30,&jbcpos);

      if(stricmp(cfgword,"AREA")==0 || stricmp(cfgword,"NETMAIL")==0)
      {
         skiparea=FALSE;

         if(jbstrcpy(buf,cfgbuf,100,&jbcpos))
         {
            for(area=(struct Area *)cfg->AreaList.First;area;area=area->Next)
               if(stricmp(buf,area->Tagname)==0) break;

				/* Area has been changed */	

            if(area && area->changed)
            {
               skiparea=TRUE;
               WriteArea(area,newfh);
               area->changed=FALSE;
            }
				
				/* Area has been removed */
				
				if(!area)
				{
					skiparea=TRUE;
				}
         }
      }

      if(stricmp(cfgword,"NODE")==0)
      {
         skipnode=FALSE;

         if(jbstrcpy(buf,cfgbuf,100,&jbcpos))
         {
            if(Parse4D(buf,&n4d))
            {
               for(cnode=(struct ConfigNode *)cfg->CNodeList.First;cnode;cnode=cnode->Next)
                  if(Compare4D(&n4d,&cnode->Node)==0) break;

               if(cnode && cnode->changed)
               {
                  skipnode=TRUE;
                  WriteNode(cnode,newfh);
                  cnode->changed=FALSE;
               }
            }
         }
      }

      dontwrite=FALSE;

      if(skiparea)
      {
         for(c=0;areakeywords[c];c++)
            if(stricmp(cfgword,areakeywords[c])==0) dontwrite=TRUE;
      }

      if(skipnode)
      {
         for(c=0;nodekeywords[c];c++)
            if(stricmp(cfgword,nodekeywords[c])==0) dontwrite=TRUE;
      }

      if(!dontwrite)
         osPuts(newfh,cfgbuf);
   }

   for(area=(struct Area *)cfg->AreaList.First;area;area=area->Next)
      if(area->changed)
      {
		   osFPrintf(newfh,"\n");
         WriteArea(area,newfh);
         area->changed=FALSE;
      }

   osClose(oldfh);
   osClose(newfh);

   osDelete(cfgbak);
   osRename(cfg->filename,cfgbak);
   osRename(cfgtemp,cfg->filename);

   cfg->changed=FALSE;

   return(TRUE);
}


void FreeConfig(struct Config *cfg)
{
   struct Area *area;
   struct Aka *aka;
   struct ConfigNode *cnode;

   /* Config */

   for(area=(struct Area *)cfg->AreaList.First;area;area=area->Next)
   {
      jbFreeList(&area->TossNodes);
      jbFreeList(&area->BannedNodes);
   }

   for(aka=(struct Aka *)cfg->AkaList.First;aka;aka=aka->Next)
   {
      jbFreeList(&aka->AddList);
      jbFreeList(&aka->RemList);
   }

   for(cnode=(struct ConfigNode *)cfg->CNodeList.First;cnode;cnode=cnode->Next)
      jbFreeList(&cnode->RemoteAFList);

   jbFreeList(&cfg->AkaList);
   jbFreeList(&cfg->AreaList);
   jbFreeList(&cfg->CNodeList);
   jbFreeList(&cfg->PackerList);
   jbFreeList(&cfg->RouteList);
   jbFreeList(&cfg->RouteList);
   jbFreeList(&cfg->FileAttachList);
   jbFreeList(&cfg->BounceList);
   jbFreeList(&cfg->ChangeList);
   jbFreeList(&cfg->RemapList);
   jbFreeList(&cfg->RemapNodeList);
   jbFreeList(&cfg->RobotList);
   jbFreeList(&cfg->AreaFixList);
   jbFreeList(&cfg->ArealistList);
}
