package body Xfw_Naming is

   procedure Proc_1 is
   begin
      null;
   end Proc_1;

   procedure Proc_1 (I : in Integer) is
   begin
      null;
   end Proc_1;

   function F (X, Y : Integer) return Integer is begin return 0; end F;
   function F (X : Float) return access Integer is begin return null; end F;
   procedure P (X : access Integer) is null;
   procedure P (X : access procedure) is null;
   procedure P (X : access procedure; Y : access procedure (X:access Integer)) is null;
   procedure P (X : access procedure (X, Y : Float)) is null;
   procedure P (X : access function return Float) is null;
   procedure P (X : access function (X : access  procedure (X, Y:Float))
                           return access function
                                         return access function (F:Float)
                                                       return access Integer) is null;

   I : aliased Integer;
   AI : access Integer;
   procedure P_No_Param is null;
   procedure Two_Floats (X, Y : Float) is null;
   function  F_No_Param return Float is begin return 1.0; end;
   function  F_Acc_Int (F : Float) return access Integer is begin return I'Access; end;
   function  F_Fonc return access function (F:Float) return access Integer is begin return F_Acc_Int'Access; end;
   function  F_Proc_Fonc (X : access  procedure (X, Y:Float))
                           return access function
                                         return access function (F:Float)
                                                       return access Integer is begin return F_Fonc'Access; end;
begin
   I  := F (0,0);  -- T1
   AI := F (0.0);  -- T2
   P (I'Access);   -- T3
   P (P_No_Param'Access);  -- T4
   P (P_No_Param'Access, P'access); -- T5, T3
   P (Two_Floats'Access);  -- T6
   P (F_No_Param'Access);  -- T7
   P (F_Proc_Fonc'Access); -- T8
end Xfw_Naming;
