settings
{
	modes
	{
		Assault
		{
			Limit Roles: 2 Of Each Role Per Team
		}

		Control
		{
			Limit Roles: 2 Of Each Role Per Team
		}

		Escort
		{
			Limit Roles: 2 Of Each Role Per Team
		}

		Hybrid
		{
			Limit Roles: 2 Of Each Role Per Team
		}
	}
}

rule("Rule Name")
{
	event
	{
		Ongoing - Each Player;
		All;
		All;
	}

	conditions
	{
		Is Game In Progress == True;
	}

	actions
	{
		Set Ability 1 Enabled(Event Player, True);
	}
}

rule("Rule Name 2")
{
	event
	{
		Player Dealt Damage;
		Team 2;
		Slot 6;
	}
}