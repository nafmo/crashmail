/* Support for Squish style message bases for CrashMail II
 * (c) Copyright 1999 Peter Karlsson <pk@abc.se>
 */

#include "crashmail.h"

#include <msgapi.h>

struct _stamp asciiToStamp(const char *datetime);
struct _stamp nowstamp(void);

struct sqflag
{
   ulong sqflagbit;
   ulong fidoflagbit;
};

static const char months[] = "JanFebMarAprMayJunJulAugSepOctNovDec";

bool squish_beforefunc(void)
{
    struct _minf minf;
    memset(&minf, 0, sizeof (minf));
    if (-1 == MsgOpenApi(&minf))
        return FALSE;
    else
        return TRUE;
}

bool squish_afterfunc(bool success)
{
    if (-1 == MsgCloseApi())
        return FALSE;
    else
        return TRUE;
}

bool squish_importfunc(struct MemMessage *mm,struct Area *area)
{
    HAREA harea;
    HMSG hmsg;

    /* Open area */
    harea = MsgOpenArea(area->Path, MSGAREA_CRIFNEC, MSGTYPE_SQUISH);

    if (NULL == harea)
    {
        if (MERR_BADF == msgapierr)
            LogWrite(1, SYSTEMERR, "Squish area damaged \"%s\"", area->Path);
        return FALSE;
    }

    /* Lock area for improved speed */
    MsgLock(harea);

    /* Import the message */
    hmsg = MsgOpenMsg(harea, MOPEN_CREATE, 0);
    if (NULL == hmsg)
    {
        LogWrite(1, SYSTEMERR, "Unable to write to Squish area \"%s\"",
                 area->Path);
    }
    else
    {
        XMSG xmsg;
        bool iskludge, wascr;
        ulong msgsize, kludgesize, ofs, cr, c;
        byte *kludge_p, *temp_p, *end_p;
        uchar *seenby_p;
        struct TextChunk *chunk, *chunkstart;
        struct Path *path;

        memset(&xmsg, 0, sizeof (XMSG));

        /* Fill header */
        strcpy(xmsg.from, mm->From);
        strcpy(xmsg.to, mm->To);
        strcpy(xmsg.subj, mm->Subject);
        if (mm->Area[0] == 0)
        {
            /* Netmail addresses */
            xmsg.orig.zone  = mm->OrigNode.Zone;
            xmsg.orig.net   = mm->OrigNode.Net;
            xmsg.orig.node  = mm->OrigNode.Node;
            xmsg.orig.point = mm->OrigNode.Point;
            xmsg.dest.zone  = mm->DestNode.Zone;
            xmsg.dest.net   = mm->DestNode.Net;
            xmsg.dest.node  = mm->DestNode.Node;
            xmsg.dest.point = mm->DestNode.Point;
        }
        else
        {
            /* In echomail, we use the PKT addresses */
            xmsg.orig.zone  = mm->PktOrig.Zone;
            xmsg.orig.net   = mm->PktOrig.Net;
            xmsg.orig.node  = mm->PktOrig.Node;
            xmsg.orig.point = mm->PktOrig.Point;
            xmsg.dest.zone  = mm->PktDest.Zone;
            xmsg.dest.net   = mm->PktDest.Net;
            xmsg.dest.node  = mm->PktDest.Node;
            xmsg.dest.point = mm->PktDest.Point;
        }
        xmsg.date_written = asciiToStamp(mm->DateTime);
        xmsg.date_arrived = nowstamp();
        xmsg.utc_ofs = 0;
        strcpy(xmsg.__ftsc_date, mm->DateTime);

        /* Attributes are the same as in the PKT file */
        xmsg.attr = mm->Attr;
    
        /* Count amount of kludges (before body) */
        iskludge = TRUE;
        wascr = TRUE;
        kludgesize = 0;
        cr = 0;
        for (chunk = (struct TextChunk *) mm->TextChunks.First;
             chunk && iskludge; chunk = chunk->Next)
        {
            for (c = 0; c < chunk->Length && iskludge; c ++)
            {
                if (wascr)
                {
                    if (chunk->Data[c] != 1)
                        iskludge = FALSE;
                    else
                        kludgesize ++;
                    wascr = FALSE;
                }
                else if (13 == chunk->Data[c])
                {
                    wascr = TRUE;
                    cr ++;
                }
                else
                    kludgesize ++;
            }
        }

        /* Move kludges over to a kludge buffer */
        /* (If we fail to allocate, we put all in the body, which might */
        /*  not be the best of stragegies, but at least is one) */
        kludge_p = osAlloc(kludgesize + 1);
        ofs = 0;
        chunk = (struct TextChunk *) mm->TextChunks.First;
        if (kludge_p)
        {
            temp_p = kludge_p;
            end_p = temp_p + kludgesize;

            while (chunk && temp_p < end_p)
            {
                for (ofs = 0; ofs < chunk->Length && temp_p < end_p; ofs ++)
                {
                    if (13 != chunk->Data[ofs])
                        *(temp_p ++) = chunk->Data[ofs];
                }
                if (temp_p < end_p) chunk = chunk->Next;
            }
            ofs ++;

            /* Terminate with null */
            *temp_p = 0;
        }
        else
        {
            kludgesize = 0;
        }
        chunkstart = chunk;

        /* Find out the total length of the body */
        msgsize = 0;

        for (chunk = (struct TextChunk *) mm->TextChunks.First; chunk;
             chunk = chunk->Next)
            msgsize += chunk->Length;

        msgsize -= kludgesize + cr;

        /* SEEN-BY */
        seenby_p = NULL;
        if (config.cfg_Flags & CFG_IMPORTSEENBY)
            if (seenby_p == mmMakeSeenByBuf(&mm->SeenBy))
                msgsize += strlen(seenby_p);

        /* PATH */
        for (path = (struct Path *) mm->Path.First; path; path=path->Next)
            for (c = 0; c < path->Paths; c ++)
                msgsize += strlen(path->Path[c]) + 8;

        /* Write message in chunks */

        MsgWriteMsg(hmsg, 0, &xmsg, NULL, 0, msgsize, kludgesize, kludge_p);
        if (kludge_p) osFree(kludge_p);

        chunk = chunkstart;
        while (chunk)
        {
            MsgWriteMsg(hmsg, 1, NULL, &(chunk->Data[ofs]),
                        chunk->Length - ofs, msgsize, 0, NULL);

            chunk = chunk->Next;
            ofs = 0;
        }

        /* SEEN-BY */
        if ((config.cfg_Flags & CFG_IMPORTSEENBY) && mm->Area[0] != 0)
        {
            seenby_p = mmMakeSeenByBuf(&mm->SeenBy);
            if (seenby_p)
            {
                MsgWriteMsg(hmsg, 1, NULL, seenby_p, strlen(seenby_p),
                            msgsize, 0, NULL);
                osFree(seenby_p);
            }
            else
                return FALSE;
        }

        /* PATH */
        for (path = (struct Path *) mm->Path.First; path; path = path->Next)
            for (c = 0; c < path->Paths; c ++)
            {
                MsgWriteMsg(hmsg, 1, NULL, "\x01PATH: ", 7, msgsize, 0, NULL);
                MsgWriteMsg(hmsg, 1, NULL, path->Path[c],
                            strlen(path->Path[c]), msgsize, 0, NULL);
                MsgWriteMsg(hmsg, 1, NULL, "\x0d", 1, msgsize, 0, NULL);
            }

        MsgCloseMsg(hmsg);
    }

    /* Unlock area */
    MsgUnlock(harea);

    /* Close area */
    if (-1 == MsgCloseArea(harea))
        return FALSE;

    return TRUE;
}


bool squish_ExportSquishNum(HAREA harea,
                            struct Area *area,
                            ulong num,
                            bool (*handlefunc)(struct MemMessage *mm))
{
    HMSG hmsg;
    XMSG xmsg;
    struct MemMessage *mm;
    uchar buf[200];
    byte *kludges_p, *start_p, *end_p, *cur_p, *body_p;
    char *ctrl_p;
    bool wascr, stay;
    ulong msgsize, kludgesize;

    mm = mmAlloc();
    if (!mm)
        return FALSE;

    if (isrescanning)
    {
        hmsg = MsgOpenMsg(harea, MOPEN_RW, num);
    }
    else
    {
        hmsg = MsgOpenMsg(harea, MOPEN_READ, num);
    }

    if (NULL == hmsg)
    {
        if (MERR_BADF == msgapierr)
        {
            LogWrite(1, SYSTEMERR, "Squish area damaged \"%s\"", area->Path);
            return FALSE;
        }
        if (MERR_NOENT == msgapierr)
        {
            /* If we get NOENT, it just means that the specified number
             * was invalid. We skip this error
             */
            return TRUE;
        }
        return FALSE;
    }

    /* Fetch XMSG header and kludges */
    ctrl_p = osAlloc(kludgesize);
    if (!ctrl_p)
    {
        nomem = TRUE;
        MsgCloseMsg(hmsg);
        return FALSE;
    }

    MsgReadMsg(hmsg, &xmsg, 0, 0, NULL, kludgesize, ctrl_p);

    if (area->Flags & AREA_NETMAIL)
    {
        if (xmsg.attr & MSGSENT && !isrescanning)
        {
            osFree(ctrl_p);
            MsgCloseMsg(hmsg);
            return TRUE;
        }

        /* Get address information from INTL/FMPT/TOPT */
        ConvertControlInfo(ctrl_p, &xmsg.orig, &xmsg.dest);
    }
    else
    {
        if (xmsg.attr & MSGSCANNED && !isrescanning)
        {
            osFree(ctrl_p);
            MsgCloseMsg(hmsg);
            return TRUE;
        }
    }

    msgsize = MsgGetTextLen(hmsg);
    kludgesize = MsgGetCtrlLen(hmsg);

    if (-1 == msgsize || -1 == kludgesize)
    {
        MsgCloseMsg(hmsg);
        return FALSE;
    }

    mm->OrigNode.Zone  = xmsg.orig.zone;
    mm->OrigNode.Net   = xmsg.orig.net;
    mm->OrigNode.Node  = xmsg.orig.node;
    mm->OrigNode.Point = xmsg.orig.point;
    mm->DestNode.Zone  = xmsg.dest.zone;
    mm->DestNode.Net   = xmsg.dest.net;
    mm->DestNode.Node  = xmsg.dest.node;
    mm->DestNode.Point = xmsg.dest.point;

   if (area->Flags & AREA_NETMAIL)
      strcpy(mm->Area, "");
   else
      strcpy(mm->Area, area->Tagname);

    mystrncpy(mm->To, xmsg.to, 36);
    mystrncpy(mm->From, xmsg.from, 36);
    mystrncpy(mm->Subject, xmsg.subj, 72);

    if (xmsg.date_written.date.mo < 1 || xmsg.date_written.date.mo > 12)
        xmsg.date_written = nowstamp();

    sprintf(mm->DateTime, "%02d %c%c%c %02d  %02d:%02d:%02d",
            xmsg.date_written.date.da,
            months[xmsg.date_written.date.mo * 3 - 3],
            months[xmsg.date_written.date.mo * 3 - 2],
            months[xmsg.date_written.date.mo * 3 - 1],
            (xmsg.date_written.date.yr + 80) % 100,
            xmsg.date_written.time.hh,
            xmsg.date_written.time.mm,
            xmsg.date_written.time.ss);

    /* Attributes are the same as in the Squish database, but some shouldn't */
    /* be exported                                                           */
    mm->Attr = xmsg.attr & 
               (FLAG_PVT|FLAG_CRASH|FLAG_FILEATTACH|FLAG_FILEREQ|FLAG_RREQ|
                FLAG_IRRR|FLAG_AUDIT|FLAG_HOLD);
    mm->Cost = 0;
    mm->msgnum = num;

    /* Add our own kludges */
    if (area->Flags & AREA_NETMAIL)
    {
        if (mm->OrigNode.Zone != mm->DestNode.Zone ||
            (config.cfg_Flags & CFG_FORCEINTL))
        {
            sprintf(buf,"\x01" "INTL %u:%u/%u %u:%u/%u\x0d",
                    mm->DestNode.Zone,
                    mm->DestNode.Net,
                    mm->DestNode.Node,
                    mm->OrigNode.Zone,
                    mm->OrigNode.Net,
                    mm->OrigNode.Node);

            mmAddLine(mm, buf);
        }
        if (mm->OrigNode.Point)
        {
            sprintf(buf, "\x01" "FMPT %u\x0d", mm->OrigNode. Point);
            mmAddLine(mm, buf);
        }

        if (mm->DestNode.Point)
        {
            sprintf(buf,"\x01" "TOPT %u\x0d", mm->DestNode. Point);
            mmAddLine(mm, buf);
        }
    }

    if((config.cfg_Flags & CFG_ADDTID) && !isrescanning)
        AddTID(mm);

    if(isrescanning)
    {
        sprintf(buf, "\x01RESCANNED %u:%u/%u.%u\x0d",
                area->Aka->Node.Zone,
                area->Aka->Node.Net,
                area->Aka->Node.Node,
                area->Aka->Node.Point);
        mmAddLine(mm,buf);
    }

    /* Save kludges, except for INTL, FMPT, TOPT, if present */
    kludges_p = CvtCtrlToKludge(ctrl_p);
    start_p = NULL;
    stay = TRUE;
    for (cur_p = kludges_p; stay; cur_p ++)
    {
        if (1 == *cur_p || 0 == *cur_p)
        {
            if (0 == *cur_p)
                stay = FALSE;

            if (start_p)
            {
                *cur_p = 0;
                mmAddLine(mm, start_p);
                start_p = cur_p + 1;
            }
        }
    }

    osFree(kludges_p);
    osFree(ctrl_p);

    /* Fetch body and save */
    body_p = osAlloc(msgsize + 1);
    if (!body_p)
    {
        nomem = TRUE;
        MsgCloseMsg(hmsg);
        return FALSE;
    }
    body_p[msgsize] = 0; /* Not zero terminated input */

    MsgReadMsg(hmsg, NULL, 0, msgsize, body_p, 0, NULL);

    start_p = body_p;
    end_p = &body_p[msgsize];
    for (cur_p = body_p; cur_p <= end_p; cur_p ++)
    {
        if (wascr)
        {
            if (10 == *cur_p) cur_p ++;
            start_p = cur_p;
            wascr = FALSE;
        }

        if (0 == *cur_p || 13 == *cur_p)
        {
            *cur_p = 0;
            mmAddLine(mm, start_p);
            wascr = TRUE;
        }
    }

    osFree(body_p);

    if(!(*handlefunc)(mm))
    {
        mmFree(mm);
        MsgCloseMsg(hmsg);
        return FALSE;
    }

    mmFree(mm);

    if (!isrescanning)
    {
        scan_total ++;

        /* Remove messages marked with kill/sent if allowed */
        if ((config.cfg_Flags & CFG_ALLOWKILLSENT) &&
            (xmsg.attr & MSGKILL) && (area->Flags & AREA_NETMAIL))
        {
            MsgCloseMsg(hmsg);
            if (0 == MsgKillMsg(harea, num))
            {
			    LogWrite(2, TOSSINGINFO, "Deleting message with KILLSENT flag");
            }
            else
            {
                LogWrite(1, SYSTEMERR, "Unable to delete KILLSENT message");
            }
        }
        else
        {
            /* Mark netmail as sent, echomail as scanned */
            if (area->Flags & AREA_NETMAIL)
            {
                xmsg.attr |= MSGSENT;
            }
            else
            {
                xmsg.attr |= MSGSCANNED;
            }

            /* Update flags */
            MsgWriteMsg(hmsg, 0, &xmsg, NULL, 0, msgsize, 0, NULL);

            /* Close message */
            MsgCloseMsg(hmsg);
        }
    }

    return TRUE;
}

bool squish_rescanfunc(struct Area *area, ulong max,
                       bool (*handlefunc)(struct MemMessage *mm))
{
    ulong start, end, msgn;

    HAREA harea;

    /* Open area */
    harea = MsgOpenArea(area->Path, MSGAREA_CRIFNEC, MSGTYPE_SQUISH);

    if (NULL == harea)
    {
        if (MERR_BADF == msgapierr)
            LogWrite(1, SYSTEMERR, "Squish area damaged \"%s\"", area->Path);
        return FALSE;
    }

    /* Lock area for improved speed */
    MsgLock(harea);

    end = MsgGetHighMsg(harea);

    if (isrescanning)
    {
        start = 1;
    }
    else
    {
        start = MsgGetHighWater(harea) + 1;
    }

    if (max != 0 && end - start > max)
    {
        start = end - max + 1;
    }

    for (msgn = start; msgn <= end; msgn ++)
    {
        if (!squish_ExportSquishNum(harea, area, msgn, handlefunc))
            return FALSE;
    
        if (ctrlc)
            return FALSE;
    }

    if (!isrescanning)
    {
        MsgSetHighWater(harea, end);
    }

    /* Unlock area */
    MsgUnlock(harea);

    /* Close area */
    if (-1 == MsgCloseArea(harea))
        return FALSE;

    return TRUE;
}

bool squish_exportfunc(struct Area *area,
                       bool (*handlefunc)(struct MemMessage *mm))
{
    return squish_rescanfunc(area, 0, handlefunc);
}

struct _stamp asciiToStamp(const char *datetime)
{
    struct _stamp stamp;
    int yr, mo, da, hh, mm, ss;
    char month[4] = { 0,0,0,0 }, *c_p;

    if (' ' == datetime[2])
    { // "Dd Mmm Yy  HH:MM:SS" 
        sscanf(datetime, "%d %s %d %d:%d:%d",
               &da, month, &yr, &hh, &mm, &ss);
    }
    else if (' ' == datetime[3])
    { // "Www Dd Mmm Yy HH:MM"
        sscanf(&datetime[4], "%d %s %d %d:%d",
               &da, month, &yr, &hh, &mm);
        ss = 0;
    }
    else
    {
        memset(&stamp, 0, sizeof (stamp));
        return stamp;
    }

    // Check month
    c_p = strstr(months, month);
    if (c_p) mo = ((int) (c_p - months)) / 3 + 1;
    else mo = 1;

    /*FIXME: Year range of 1980-2079: 80-99 => 1980-1999, 00-79 => 2000-2079*/
    /*       Should use a sliding window to determine year                  */
    stamp.date.yr = (yr < 80) ? yr + 20 : yr - 80;
    stamp.date.mo = mm;
    stamp.date.da = da;
    stamp.time.hh = hh;
    stamp.time.mm = mm;
    stamp.time.ss = ss;
    return stamp;
}

struct _stamp nowstamp(void)
{
    time_t tt;
    struct tm *tm_p;
    struct _stamp rc;

    tt = time(NULL);
    tm_p = localtime(&tt);
    rc.date.yr = tm_p->tm_year - 80; /*Overflow in 2108 (_stamp limit)*/
    rc.date.mo = tm_p->tm_mon + 1;
    rc.date.da = tm_p->tm_mday;
    rc.time.hh = tm_p->tm_hour;
    rc.time.mm = tm_p->tm_min;
    rc.time.ss = tm_p->tm_sec;
    
    return rc;
}
