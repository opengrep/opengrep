(*
   Unit tests for Domainslib_
*)

let test_num_domains_of_jobs () =
  let cpus = Domainslib_.get_cpu_count () in
  let default = Domainslib_.default_num_domains () in
  Alcotest.(check int) "-j 1 runs on one core" 1
    (Domainslib_.num_domains_of_jobs 1);
  Alcotest.(check int) "-j above the cores runs on all of them" cpus
    (Domainslib_.num_domains_of_jobs (cpus + 5));
  Alcotest.(check int) "-j 0 runs on the default" default
    (Domainslib_.num_domains_of_jobs 0);
  Alcotest.(check int) "-j below 0 runs on the default" default
    (Domainslib_.num_domains_of_jobs (-3))

let tests =
  Testo.categorize "Domainslib_"
    [
      Testo.create "-j selects at most the machine's cores, and the default for 0 or less"
        test_num_domains_of_jobs;
    ]
