bool squish_beforefunc(void);
bool squish_afterfunc(bool success);
bool squish_importfunc(struct MemMessage *mm,struct Area *area);
bool squish_exportfunc(struct Area *area,bool (*handlefunc)(struct MemMessage *mm));
bool squish_rescanfunc(struct Area *area,ulong max,bool (*handlefunc)(struct MemMessage *mm));
