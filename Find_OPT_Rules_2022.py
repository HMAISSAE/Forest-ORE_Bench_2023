import gurobipy as gp
from gurobipy import GRB
import pandas as pd
from time import time
import inspect
# import os
# print(os.getcwd())
def find_opt_rules(RulesMetrics_location, RulesCoverOk_location, RulesCoverNok_location, InitError,
                   W0=1, W1=1, W2=0.1, W3=0.05, MaxCover=3, MaxOverlap=0.25, Alpha=0.025, Beta=0.025):
    """
    Find the optimal number of rules needed to interpret Random Forest model while considering diverse individual
    and collaborative factors: predictive performance, coverage, complexity (attributes number and levels numbers),
    and overlapping.

    Parameters
    ----------

    RulesMetrics_location: str
      The location (path) of the csv file describing the preselected rules information and metrics.
      This data frame (m rows and 10 columns) provides for each of the m rules: the rule “id”,
      its “confidence”, its “support”, the size of the variables used “attributes_nbr”, the size of the
      modalities used “levels_nbr”, the size of the variables scaled (values between 0 and 1) “att_nbr_scaled”,
      the size of the modalities scaled (values between 0 and 1) “levels_nbr_scaled”, the variables used in the
      condition “attributes”, the predicted target “Ypred”, and the expression of the condition “condition”.
    RulesCoverOk_location: str
      The location (path) of the csv file providing the data frame (n rows and m columns) of the preselected rules
      correct coverage. It is a binary data frame where each cell CovOk[i,j] = 1 if rule j covers instance i and predict
      it correctly, 0 otherwise.
    RulesCoverNok_location: str
      The location (path) of the csv file providing the data frame (n rows and m columns) of the preselected rules
      incorrect coverage. It is a binary data frame where each cell CovNok[i,j] = 1 if rule j covers instance i and does
      not predict it correctly, 0 otherwise.
    InitError: float
      ∈ [0 ,1]. The error in prediction (training set) of the m preselected rules
    w0, w1, w2, w3: float
      ∈ [0 ,1]. Weights used in the objective function. Default parameters used are respectively:  1, 1, 0.1, 0.05
    MaxCover: int
      ∈ {1,2…10}. The upper bound for the number of rules to which an instance can belong to. The default parameter is 3.
    MaxOverlap: float
      ∈ [0 ,1]. The upper bound for the overall overlap ratio(ratio of the instances belonging to 2 rules or more).
      The default parameter is 0.25.
    Alpha: float
      The upper bound for the loss in overall accuracy compared to the accuracy of Random Forest classifier.
      The default parameter is 0.025
    Beta: float
      The upper bound for the loss in overall coverage compared to the coverage of the preselected rules
      (initial coverage equals 1). The default parameter is 0.025.

    Returns
    ----------

    summary: DataFrame
      Summary of the optimization parameters and results.
    Rules: list
      List of optimally selected rules ids.
    not_covered_instances: list
      List of instance ids not covered by the optimally selected rules.
    incorrectly_covered_instances: list
      List of incorrectly covered instances ids.
    overlapping_instances: list
      List of overlapping instances ids.

    """

    start_time = time()
    # Import data
    with open(RulesMetrics_location, 'r') as input0_data_file:
        input0_data = input0_data_file.read()

    lines_metrics = input0_data.split('\n')

    df_cov_ok = pd.read_csv(RulesCoverOk_location)
    df_cov_nok = pd.read_csv(RulesCoverNok_location)
    rules_nbr = len(df_cov_ok.columns)
    instances_nbr = len(df_cov_ok)

    # Parameters : Rules metrics

    rules, rule_id, confidence, support, var_ratio, levels_ratio, var_length, levels_length = gp.multidict(
        {0: [0, 0, 0, 0, 0, 0, 0]})
    # coverage_set[3](3)
    line_metric0 = lines_metrics[1]
    metrics_parts0 = line_metric0.split(sep=",")
    rule_id[0] = int(metrics_parts0[0])
    confidence[0] = float(metrics_parts0[1])
    support[0] = float(metrics_parts0[2])
    var_ratio[0] = float(metrics_parts0[6])
    levels_ratio[0] = float(metrics_parts0[7])
    var_length[0] = int(metrics_parts0[4])
    levels_length[0] = int(metrics_parts0[5])


    for l in range(1, rules_nbr):
        line_metric = lines_metrics[l + 1]
        metrics_parts = line_metric.split(sep=",")
        rules.append(l)
        rule_id[l] = int(metrics_parts[0])
        confidence[l] = float(metrics_parts[1])
        support[l] = float(metrics_parts[2])
        var_ratio[l] = float(metrics_parts[6])
        levels_ratio[l] = float(metrics_parts[7])
        var_length[l] = int(metrics_parts[4])
        levels_length[l] = int(metrics_parts[5])
    t = 5
    rules[t], rule_id[t], confidence[t], support[t], var_ratio[t], levels_ratio[t], var_length[t], levels_length[t]
    init_error = InitError

    w0 = W0
    w1 = W1
    w2 = W2
    w3 = W3
    maxcover = MaxCover
    maxoverlap = MaxOverlap
    # max_rules_nbr=30
    alpha = Alpha  # (error tolerance)
    beta = Beta  # (coverage tolerance)


    # MIP  model formulation
    m = gp.Model("rule_cover")
    # is_selected(j) equals 1 if a rule j is selected, 0 otherwise
    is_selected = m.addVars(rules_nbr, vtype=GRB.BINARY, name="is_selected")
    # is_covered(i) equals 1 if instance i  is covered, 0 otherwise
    is_covered = m.addVars(instances_nbr, vtype=GRB.BINARY, name="is_covered")
    # is_error(i) equals 1 if an instance i is incorrectly predicted, 0 otherwise
    is_error = m.addVars(instances_nbr, vtype=GRB.BINARY, name="is_error")
    # is_overlap(i) equals 1 if an instance i belongs to more than 2 rules, 0 otherwise
    is_overlap = m.addVars(instances_nbr, vtype=GRB.BINARY, name="is_overlap")

    # m.setObjective(gp.quicksum(is_selected), GRB.MINIMIZE)
    m.setObjective(gp.quicksum(is_selected[j] * (
            1 + w0 * (1 - confidence[j]) + w1 * (1 - support[j]) + w2 * var_ratio[j] + w3 * levels_ratio[j]) for j
                               in rules), GRB.MINIMIZE)

    # max rules constraints: coverded instances are explained by at most "max_rules_nbr" rules
    # m.addConstr((gp.quicksum(is_selected) <= max_rules_nbr), name="max_rules_nbr")

    # maxcover constraint: each instance is covered by at most "maxcover" rules
    # the corresponding constraint can be written as following:
    # m.addConstrs((gp.quicksum(is_selected[j] * (df_cov_ok.iloc[i, j] + df_cov_nok.iloc[i, j]) for j in rules) <= maxcover
    #               for i in range(instances_nbr)), name="maxcover")
    # However, "coverconstr1" (in the following) is strongest

    # error in prediction constraint: The selected rules error in predicting covered instances do not
    # exceed (initial error)*(1+"alpha") or (initial error+)"alpha".
    # given equation "is_error1" and "is_error2", an instance is considered predicted incorrectly if:
    # - The instance is covered, and the rules prediction(voting) does not match the reel target value, or there is tie.
    # - The instance is not covered

    m.addConstrs((gp.quicksum(
        is_selected[j] * (df_cov_ok.iloc[i, j] - df_cov_nok.iloc[i, j]) for j in rules) <= maxcover * (1 - is_error[i])
                  for i in range(instances_nbr)), name="iserror1")

    m.addConstrs((gp.quicksum(is_selected[j] * (df_cov_ok.iloc[i, j] - df_cov_nok.iloc[i, j]) for j in rules) >= 1 -
                  is_error[i] - maxcover * is_error[i] for i in range(instances_nbr)), name="iserror2")

    # m.addConstr((gp.quicksum(is_error) <= init_error*(1+alpha)*instances_nbr), name="maxerror")
    # m.addConstr((gp.quicksum(is_error) - gp.quicksum(1 - is_covered[i] for i in range(instances_nbr)) <= init_error * (
    #         1 + alpha) * gp.quicksum(is_covered)), name="maxerror")
    m.addConstr((gp.quicksum(is_error) - gp.quicksum(1 - is_covered[i] for i in range(instances_nbr)) <= (
                init_error + alpha) * gp.quicksum(is_covered)), name="maxerror")
    # min coverage constraint: Selected rules cover at least (1-"beta")% of our population

    m.addConstrs((gp.quicksum(
        is_selected[j] * (df_cov_ok.iloc[i, j] + df_cov_nok.iloc[i, j]) for j in rules) <= maxcover * is_covered[i] for
                  i in range(instances_nbr)), name="coverconstr1")

    m.addConstrs(
        (gp.quicksum(is_selected[j] * (df_cov_ok.iloc[i, j] + df_cov_nok.iloc[i, j]) for j in rules) >= is_covered[i]
         for i in range(instances_nbr)), name="coverconstr2")

    m.addConstr((gp.quicksum(is_covered) >= 1*(1 - beta) * instances_nbr), name="mincover")

    # min coverage constraint:number of constraints = 1+2*instances_nbr

    # max overall overlap constraint: the rate of instances belonging to more than 2 rules do not exceed 100*"maxovelap" %
    m.addConstrs((gp.quicksum(is_selected[j] * (df_cov_ok.iloc[i, j] + df_cov_nok.iloc[i, j]) for j in rules) <= 1 -
                  is_overlap[i] + maxcover * is_overlap[i] for i in range(instances_nbr)), name="operlap1")

    m.addConstrs((gp.quicksum(is_selected[j] * (df_cov_ok.iloc[i, j] + df_cov_nok.iloc[i, j]) for j in rules) >= 2 *
                  is_overlap[i] for i in range(instances_nbr)), name="overlap2")

    # m.addConstr((gp.quicksum(is_overlap) <= maxovelap*instances_nbr), name="maxoverlap")
    m.addConstr((gp.quicksum(is_overlap) <= maxoverlap * gp.quicksum(is_covered)), name="maxoverlap")

    # all instances are covered
    # m.addConstrs((gp.quicksum(is_selected[j] * (df_cov_ok.iloc[i, j] + df_cov_nok.iloc[i, j]) for j in rules) >= 1
    #               for i in range(instances_nbr)), name="allcover")

    # Save model formulation for inspection
    # m.write('workforce.lp')

    # Optimize
    m.optimize()
    run_time = m.Runtime

    optim_status = m.status
    # The Status attribute  provides  optimization status
    if optim_status == GRB.Status.INF_OR_UNBD or optim_status == GRB.Status.INFEASIBLE or optim_status == GRB.Status.UNBOUNDED:
        raise ValueError('The model cannot be solved because it is infeasible or unbounded. Modify the tuning parameters to improve the model.')
        # return
        # print(
        #     'The model cannot be solved because it is infeasible or unbounded. Modify the tuning '
        #     'parameters to improve the model')
        # sys.exit(0)
    # If the optimization status of the model is not optimal for some other reason, we report that
    # situation.
    if optim_status != GRB.Status.OPTIMAL:
        raise ValueError('The optimization model is not optimal. Optimization was stopped with status ' + str(optim_status) + ' Modify the tuning parameters to improve the model.')
        # print('Optimization was stopped with status ' + str(status))
        # sys.exit(0)
        # return

    if optim_status == 2:
        opt_status = "Optimal"
    else:
        opt_status = "NotOptimal"

    # m.getObjective()

    # isol = [int(var.x) for var in m.getVars()]
    # selected_rules = [i for i, v in enumerate(isol) if v == 1 and i in range(rules_nbr)]
    selected_rules = [i for i, v in enumerate(is_selected) if is_selected[i].x == 1]
    id_selected_rules = [v + 1 for i, v in enumerate(selected_rules)]
    # covered_instances = [(i - rules_nbr) for i, v in enumerate(isol) if
    #                  v == 1 and i in range(rules_nbr, rules_nbr + instances_nbr)]
    covered_instances = [i for i, v in enumerate(is_covered) if is_covered[i].x == 1]
    not_covered_instances = [i for i, v in enumerate(is_covered) if is_covered[i].x == 0]
    id_not_covered_instances = [v + 1 for i, v in enumerate(not_covered_instances)]

    # incorrectly_covered_instances = [(i - rules_nbr - instances_nbr) for i, v in enumerate(isol) if
    #                              v == 1 and i in range(rules_nbr + instances_nbr, rules_nbr + 2 * instances_nbr)]
    # not_covered_instances = [(i - rules_nbr) for i, v in enumerate(isol) if
    #                      v == 0 and i in range(rules_nbr, rules_nbr + instances_nbr)]

    incorrectly_covered_instances = [i for i, v in enumerate(is_error) if is_error[i].x == 1 and i in range(instances_nbr)]
    id_incorrectly_covered_instances = [v + 1 for i, v in enumerate(incorrectly_covered_instances)]

    # overlapping_instances = [(i - rules_nbr - 2 * instances_nbr) for i, v in enumerate(isol) if
    #                      v == 1 and i in range(rules_nbr + 2 * instances_nbr, rules_nbr + 3 * instances_nbr)]
    overlapping_instances = [i for i, v in enumerate(is_overlap) if is_overlap[i].x == 1 and i in range(instances_nbr)]
    id_overlapping_instances = [v + 1 for i, v in enumerate(overlapping_instances)]

    # with open('./Rules_files/BCo_opt_not_covered_instances.txt','w') as f:
    #     f.write(' '.join([str(id) for id in id_not_coverd_instances]))

    # with open('./Rules_files/BCo_id_selected_rules.txt','w') as f:
    #     f.write(' '.join([str(id) for id in id_selected_rules]))
    #
    end_time = time()
    time_taken = end_time - start_time

    opt_parameters = pd.DataFrame({'error_tolerance': [alpha], 'coverage_tolerance': [beta],
                                   'coverage_ratio': [round(len(covered_instances) / instances_nbr, 4)],
                                   'init_error': [init_error],
                                   'covered_error_ratio': [round(
                                       (len(incorrectly_covered_instances) - len(not_covered_instances)) / len(
                                           covered_instances), 4)],
                                   'overlapping_ratio': [round(len(overlapping_instances) / len(covered_instances), 4)],
                                   'optimality': [opt_status],
                                   'opt_run_time': [round(run_time, 4)],
                                   'run_time': [round(time_taken, 4)],
                                   'init_Rules_nbr': [rules_nbr],
                                   'Rules_nbr': [len(selected_rules)]})

    # opt_parameters.to_csv(r'./Rules_files/BCo_opt_parameters.csv', index=False, header=True)

    print(f"optimization status is: {m.status}")
    print(f"run time is : {run_time} s")
    print(f"initial rules size is: {rules_nbr}")
    print(f"population size is: {instances_nbr}")
    print(f"error tolerance is: {alpha}")
    print(f"coverage tolerance is: {beta}")

    print(f"The size of selected rules is: {len(selected_rules)}")
    print(f"ids of selected rules are: {id_selected_rules}")
    print(f"coverage ratio is: {len(covered_instances) / instances_nbr}")
    # if len(covered_instances) / instances_nbr < 1:
    #     print(f"ids of not covered instances are: {id_not_covered_instances}")

    print(
        f"The size of incorrectly predicted instances is: {(len(incorrectly_covered_instances) - len(not_covered_instances))}")
    print(
        f"The covered_error_ratio is :{(len(incorrectly_covered_instances) - len(not_covered_instances)) / len(covered_instances)}")
    # if len(incorrectly_covered_instances) > 0:
    #     print(f"ids of incorrectly covered instances are: {id_incorrectly_covered_instances}")

    print(f"The size of overlapping instances is: {len(overlapping_instances)}")
    print(f"The overlapping_ratio is :{len(overlapping_instances) / len(covered_instances)}")
    # print(f"ids of overlapping instances is: {id_overlapping_instances}")

    att_totallength = 0
    levl_totallength = 0
    attl = 0
    levl = 0
    for j in selected_rules:
        attl += var_ratio[j]
        levl += levels_ratio[j]
        att_totallength += var_length[j]
        levl_totallength += levels_length[j]
    print(f"total attributes ratios is {attl}, and total levels ratios is {levl}")
    print(f"total attributes length is {att_totallength}, and total levels length is {levl_totallength}")

    for j in selected_rules:
        print(rules[j], rule_id[j], confidence[j], support[j], var_length[j], levels_length[j])

    return {'summary': opt_parameters, 'Rules': id_selected_rules, 'not_covered_instances': id_not_covered_instances,
            'incorrectly_covered_instances': id_incorrectly_covered_instances,
            "overlapping_instances": id_overlapping_instances}

# opt_solution = find_opt_rules(RulesMetrics_location = "./Rules_files/Appendicitis_Rules_metrics.csv",
#                                            RulesCoverOk_location = "./Rules_files/Appendicitis_Rules_cover_ok.csv",
#                                            RulesCoverNok_location = "./Rules_files/Appendicitis_Rules_cover_nok.csv",
#                                            InitError=0.04054054,
#                                            W0=1, W1=1, W2=0.1, W3=0.05,
#                                            MaxCover=3,
#                                            MaxOverlap = 0.5,
#                                            Alpha =0.01, Beta=0.025)
# print(opt_solution)

# opt_solution = find_opt_rules(RulesMetrics_location = "./Rules_files/XOR_Rules_metrics.csv",
#                                   RulesCoverOk_location = "./Rules_files/XOR_Rules_cover_ok.csv",
#                                   RulesCoverNok_location = "./Rules_files/XOR_Rules_cover_ok.csv",
#                                   InitError = 0,
#                                   W0 = 1, W1 = 1, W2 = 0.1, W3 = 0.05, MaxCover = 3,
#                                   MaxOverlap = 0.25, Alpha= 0.01, Beta= 0.01)

# Query number of multiple objectives, and number of solutions
