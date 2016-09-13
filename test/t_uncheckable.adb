with X_Uncheckable_Proc; -- Missing body
with X_Uncheckable;
procedure T_Uncheckable is
   use X_Uncheckable;

   -- A type whose size is unknown:
   type Unknown is range Integer'First .. Integer'Last;

   procedure Allocators                      is separate;
   procedure Case_Statement                  is separate;
   procedure Declarations                    is separate;
   procedure Directly_Accessed_Globals       is separate;
   procedure Duplicate_Initialization_Calls  is separate;
   procedure Exception_Propagation           is separate;
   procedure Generic_Aliasing                is separate;
   procedure Global_References               is separate;
   procedure Max_Call_Depth                  is separate;
   procedure Object_Declarations             is separate;
   procedure Parameter_Aliasing              is separate;
   procedure Potentially_Blocking_Operations is separate;
   procedure Record_Declarations             is separate;
   procedure Representation_Clauses          is separate;
   procedure Style                           is separate;
   procedure Unsafe_Paired_Calls             is separate;
   procedure Unsafe_Unchecked_Conversion     is separate;
   procedure Usage                           is separate;
begin
   null;
end T_Uncheckable;
