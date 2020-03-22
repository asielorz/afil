typedef struct
{
	float member_0;// x
	float member_4;// y
	float member_8;// z
} type_56;

typedef struct
{
	type_56 member_0;// min
	type_56 member_12;// max
} type_64;

int function_0(float local_0, float local_4, float local_8);
int function_1(type_64 local_0, type_56 local_24);

int function_0(float local_0, float local_4, float local_8)
{
	return (((local_0 - local_4) >= 0) && ((local_0 - local_8) <= 0));
}

int function_1(type_64 local_0, type_56 local_24)
{
	return ((function_0(local_24.member_0, local_0.member_0.member_0, local_0.member_12.member_0) && function_0(local_24.member_4, local_0.member_0.member_4, local_0.member_12.member_4)) && function_0(local_24.member_8, local_0.member_0.member_8, local_0.member_12.member_8));
}

int main(void)
{
	type_64 local_0;
	{
		type_56 expr_temp_0;

		expr_temp_0.member_0 = 0.000000f;
		expr_temp_0.member_4 = 0.000000f;
		expr_temp_0.member_8 = 0.000000f;
		local_0.member_0 = expr_temp_0;
	}
	{
		type_56 expr_temp_0;

		expr_temp_0.member_0 = 3.000000f;
		expr_temp_0.member_4 = 3.000000f;
		expr_temp_0.member_8 = 3.000000f;
		local_0.member_12 = expr_temp_0;
	}
	{
		int if_condition;
		{
			type_64 expr_temp_0;
			type_56 expr_temp_1;

			expr_temp_0 = local_0;
			expr_temp_1.member_0 = 1.000000f;
			expr_temp_1.member_4 = 1.000000f;
			expr_temp_1.member_8 = 1.000000f;
			if_condition = function_1(expr_temp_0, expr_temp_1);
		}
		if (if_condition)
		{
			return 0;
		}
		else
		{
			return 1;
		}
	}
}

