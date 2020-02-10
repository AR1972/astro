Tag file for Mirror

M000    MD      1/23/91         Revised help message
M001	SR	1/30/91		Bug #4999. In their new drop, CPS added some
				parameter checking but forgot to add /1 as 
				a valid switch in the list of switches. Added
				a new check for /1.
				Files: MIR_DTRK.ASM

M002	SR	1/30/91		Bug #5298. Made Drive_Msg length independent.
				Previously, it was length dependent because
				the drive letter was getting patched in
				with a hard-coded length. Provided an equate
				mow to get the offset to patch for the
				driveletter.
				Files: MIR_MAIN.ASM, MIR_MSG.INC

M003	SR	2/1/91		Bug #4828. Fixed Mirror so that loadhigh
				Mirror does the same as running it normally.
				Previously, there was code so that Loadhigh
				Mirror would not do the normal saving stuff
				and would load only the delete tracker.

M004    MD      2/8/91          Bug #5581.  Corrected minor error in help
                                text (mir_pmsg.asm)

M005    ???     ??/??/??        Unknown: change to MIR_MAIN.ASM

M006    DLB     02/11/91        Bugs #5677, #5678. Added explicit tests for
                                ASSIGN'd, Net, SUBST'd and JOIN'd drives
                                during initialization. Neither MIRROR or
                                Delete Tracking did these checks before.

M007	CAS	02/20/91	Adding disk resets to crit sections and
				fixing one weak spot in crit section

M008    DLB     02/20/91        Merged MIR_MAIN.ASM, MIR_MSG.INC from
                                CMD\FORMAT directory, so as to eliminate
                                duplicate files.

M009    DLB     02/24/91        Bug #5986: MIRORSAV.FIL was over-writing
                                existing files under certain conditions.
                                Also fixed detection of pre-existing sectors
                                which contain MIRORSAV.FIL signature.

M010    MD      02/26/91        Changes to CPS copyright and attribution messages

M011    MD      03/04/91        Replace accidentally deleted line from help text

m012    JAH     08-12-91        Fixed problem with exiting with interrupts disabled.

