typedef struct
{
	float member_0;// x
	float member_4;// y
	float member_8;// z
} vec3;

typedef struct
{
	vec3 member_0;// min
	vec3 member_12;// max
} aabb;

bool is_in_range__float__float__float__bool(float local_0, float local_4, float local_8);
bool intersects__aabb__vec3__bool(aabb local_0, vec3 local_24);

bool is_in_range__float__float__float__bool(float local_0, float local_4, float local_8)
{
	return (((local_0 - local_4) >= 0) && ((local_0 - local_8) <= 0));
}

bool intersects__aabb__vec3__bool(aabb local_0, vec3 local_24)
{
	return ((is_in_range__float__float__float__bool(local_24.member_0, local_0.member_0.member_0, local_0.member_12.member_0) && is_in_range__float__float__float__bool(local_24.member_4, local_0.member_0.member_4, local_0.member_12.member_4)) && is_in_range__float__float__float__bool(local_24.member_8, local_0.member_0.member_8, local_0.member_12.member_8));
}

int main(void)
{
	aabb local_0;
	{
		vec3 expr_temp_0;

		expr_temp_0.member_0 = 0.000000f;
		expr_temp_0.member_4 = 0.000000f;
		expr_temp_0.member_8 = 0.000000f;
		local_0.member_0 = expr_temp_0;
	}
	{
		vec3 expr_temp_0;

		expr_temp_0.member_0 = 3.000000f;
		expr_temp_0.member_4 = 3.000000f;
		expr_temp_0.member_8 = 3.000000f;
		local_0.member_12 = expr_temp_0;
	}
	{
		int if_condition;
		{
			aabb expr_temp_0;
			vec3 expr_temp_1;

			expr_temp_0 = local_0;
			expr_temp_1.member_0 = 1.000000f;
			expr_temp_1.member_4 = 1.000000f;
			expr_temp_1.member_8 = 1.000000f;
			if_condition = intersects__aabb__vec3__bool(expr_temp_0, expr_temp_1);
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

