;/* Text Manager routines - Supplied by the application */
externFP StartBigEdit	; void StartBigEdit(void);
externFP EndBigEdit	; void EndBigEdit(void);
externFP LinesInBuf	; ushort LinesInBuf(ushort);
externFP InsertLineBuf	; bool InsertLineBuf(ushort, ushort, ushort, char *);
externFP hbufScrap	; ushort hbufScrap(void);
externFP InsertBufInBuf ; void InsertBufInBuf(ushort, ushort, ushort);
externFP cbGetLineBuf	; ushort cbGetLineBuf(ushort, ushort, ushort, char *);
externFP FreeScrap	; void FreeScrap(void);
externFP DeleteLinesBuf ; void DeleteLinesBuf(ushort, ushort, ushort);
externFP ReplaceLineBuf ; bool ReplaceLineBuf(ushort, ushort, ushort, char *);
externFP GetLineAttrs	; LineAttr GetLineAttrs(void);
externFP fReadOnlyBuf	; bool fReadOnlyBuf(ushort);

;/* Other Application supplied routines */
externFP IsWordChar	; bool IsWordChar(byte);
