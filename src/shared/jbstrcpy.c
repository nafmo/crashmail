#include "shared/types.h"

bool jbstrcpy(uchar *dest,uchar *src,ulong maxlen,ulong *jbc)
{
   ulong d=0;
   ulong stopchar1,stopchar2;
   ulong jbcpos;

   jbcpos= *jbc;

   while(src[jbcpos]==32 || src[jbcpos]==9) jbcpos++;

   if(src[jbcpos]=='"')
   {
      jbcpos++;
      stopchar1='"';
      stopchar2=0;
   }
   else
   {
   	stopchar1=' ';
   	stopchar2=9;
   }

   while(src[jbcpos]!=stopchar1 && src[jbcpos]!=stopchar2 && src[jbcpos]!=10 && src[jbcpos]!=0 && d<maxlen-1)
   {
      if(src[jbcpos]=='\\' && src[jbcpos+1]!=0 && src[jbcpos+1]!=10)
      {
         jbcpos++;
         dest[d++]=src[jbcpos++];
      }

      else
         dest[d++]=src[jbcpos++];
   }
   dest[d]=0;
   if(src[jbcpos]==9 || src[jbcpos]==' ' || src[jbcpos]=='"') jbcpos++;

   *jbc=jbcpos;

   if(d!=0 || stopchar1=='"') return(TRUE);
   return(FALSE);
}

