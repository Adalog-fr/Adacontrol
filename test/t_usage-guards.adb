separate (T_Usage)
procedure Guards is
   procedure P (X : in Integer; Y : in out Integer) is -- procedure, X unused_param, Y Guard5
      I : Integer := 0; -- OK
      J : Integer;      -- Guard1
      K : Integer;      -- Guard1
      L : Integer;      -- Guard2
      M : Integer := 0; -- OK
      N : Integer := 0; -- OK

      Exc1 : exception; -- Guard4
      Exc2 : exception; -- Guard4

      function F (X : Integer) return Boolean is  -- Unused_Param, Guard3
      begin
         return True;
      end F;

      function G (X : Integer) return Integer is  -- Guard3
      begin
         return X;
      end G;

      task T is
         entry E1;
         entry E2;
      end T;
      task body T is
      begin
         select
            when I = 0 =>
               accept E1 do
                  I := 0;
               end E1;
         or
            when I + K = 0 =>
               accept E1 do
                  I := 0;
               end E1;
         or
            when F (K) =>
               accept E1 do
                  J := Boolean'Pos (F (0));
               end E1;
         or
            when J + L - 1 + G (K) < 0 =>
               accept E1;
               K := G (J + L);
         or
            when I = 0 or else raise Exc1 =>
            accept E1 do
               raise Exc1;
            end E1;
         or
            when I = 0 or else raise Exc2 =>
            accept E1;
            raise Exc2;
         or
            when M = 0 =>    -- Case of nested select
               accept E1 do
                  M := 0;
                  select
                     when N = 0 =>
                        accept E2 do
                           N := 1;
                        end E2;
                  end select;
               end E1;
         or
            when X = 0 =>
               accept E1;
         or
            when Y = 0 =>
               accept E1;
         end select;
      end T;
   begin
      null;
   end P;
begin
   null;
end Guards;
