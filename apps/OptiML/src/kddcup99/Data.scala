import optiml.compiler._
import optiml.library._
import optiml.shared._

import scala.virtualization.lms.common.Record

trait KDDCup99Data extends OptiMLApplication {
	val NUM_FEATURES = 41

	// discrete features
  lazy val protocol_type 		= DiscreteFeature("tcp", "udp", "icmp")
  lazy val service 					= DiscreteFeature(    
    "IRC",
    "X11",
    "Z39_50",
    "auth",
    "bgp",
    "courier",
    "csnet_ns",
    "ctf",
    "daytime",
    "discard",
    "domain",
    "domain_u",
    "echo",
    "eco_i",
    "ecr_i",
    "efs",
    "exec",
    "finger",
    "ftp",
    "ftp_data",
    "gopher",
    "hostnames",
    "http",
    "http_443",
    "icmp",
    "imap4",
    "iso_tsap",
    "klogin",
    "kshell",
    "ldap",
    "link",
    "login",
    "mtp",
    "name",
    "netbios_dgm",
    "netbios_ns",
    "netbios_ssn",
    "netstat",
    "nnsp",
    "nntp",
    "ntp_u",
    "other",
    "pm_dump",
    "pop_2",
    "pop_3",
    "printer",
    "private",
    "remote_job",
    "rje",
    "shell",
    "smtp",
    "sql_net",
    "ssh",
    "sunrpc",
    "supdup",
    "systat",
    "telnet",
    "tftp_u",
    "tim_i",
    "time",
    "urp_i",
    "uucp",
    "uucp_path",
    "vmnet",
    "whois"
  )

  lazy val flag 						= DiscreteFeature("SF")  // normal or error status of the connection
  lazy val land 						= BinaryFeature()  // 1 if connection is from/to the same host/port; 0 otherwise 
  lazy val logged_in 				= BinaryFeature()  // 1 if successfully logged in; 0 otherwise 
  lazy val root_shell 			= BinaryFeature()  // 1 if root shell is obtained; 0 otherwise
  lazy val su_attempted 		= BinaryFeature()  // 1 if ``su root'' command attempted; 0 otherwise
  lazy val is_hot_login 		= BinaryFeature()	 // 1 if the login belongs to the ``hot'' list; 0 otherwise 
  lazy val is_guest_login 	= BinaryFeature()  // 1 if the login is a ``guest''login; 0 otherwise

  // continuous features
  lazy val duration 									 = ContinuousFeature()
  lazy val src_bytes 									 = ContinuousFeature()
  lazy val dst_bytes 									 = ContinuousFeature()
  lazy val wrong_fragment 						 = ContinuousFeature()
  lazy val urgent 										 = ContinuousFeature()
  lazy val hot 												 = ContinuousFeature()
  lazy val num_failed_logins 					 = ContinuousFeature()
  lazy val num_compromised 						 = ContinuousFeature()
  lazy val num_root 									 = ContinuousFeature()
  lazy val num_file_creations 				 = ContinuousFeature()
  lazy val num_shells 								 = ContinuousFeature()
  lazy val num_access_files 					 = ContinuousFeature()
  lazy val num_outbound_cmds 					 = ContinuousFeature()
  lazy val count 											 = ContinuousFeature()
  lazy val serror_rate 								 = ContinuousFeature(max = 100)
  lazy val rerror_rate 								 = ContinuousFeature(max = 100)
  lazy val same_srv_rate 							 = ContinuousFeature(max = 100)
  lazy val diff_srv_rate 							 = ContinuousFeature(max = 100)
  lazy val srv_count 									 = ContinuousFeature()
  lazy val srv_serror_rate 						 = ContinuousFeature(max = 100)
  lazy val srv_rerror_rate 					   = ContinuousFeature(max = 100)
  lazy val srv_diff_host_rate  				 = ContinuousFeature(max = 100)
  lazy val dst_host_count							 = ContinuousFeature()
  lazy val dst_host_srv_count					 = ContinuousFeature()
  lazy val dst_host_same_srv_rate  		 = ContinuousFeature(max = 100)
  lazy val dst_host_diff_srv_rate  		 = ContinuousFeature(max = 100)
  lazy val dst_host_same_src_port_rate = ContinuousFeature(max = 100)
  lazy val dst_host_srv_diff_host_rate = ContinuousFeature(max = 100)
  lazy val dst_host_serror_rate  	 		 = ContinuousFeature(max = 100)
  lazy val dst_host_srv_serror_rate  	 = ContinuousFeature(max = 100)
  lazy val dst_host_rerror_rate  			 = ContinuousFeature(max = 100)
  lazy val dst_host_srv_rerror_rate  	 = ContinuousFeature(max = 100)

  def parseNetworkRow(row: Rep[DenseVectorView[String]]): Rep[DenseVector[Double]] = {
  	DenseVector[Double](
  		duration(row(0)),
  		protocol_type(row(1)),
  		service(row(2)),
  		flag(row(3)),
  		src_bytes(row(4)),
  		dst_bytes(row(5)),
  		land(row(6)),
  		wrong_fragment(row(7)),
  		urgent(row(8)),
  		hot(row(9)),
  		num_failed_logins(row(10)),
  		logged_in(row(11)),
  		num_compromised(row(12)),
  		root_shell(row(13)),
  		su_attempted(row(14)),
  		num_root(row(15)),
  		num_file_creations(row(16)),
  		num_shells(row(17)),
  		num_access_files(row(18)),
  		num_outbound_cmds(row(19)),
  		is_hot_login(row(20)),
  		is_guest_login(row(21)),
  		count(row(22)),
  		serror_rate(row(23)),
  		rerror_rate(row(24)),
  		same_srv_rate(row(25)),
  		diff_srv_rate(row(26)),
  		srv_count(row(27)),
  		srv_serror_rate(row(28)),
  		srv_rerror_rate(row(29)),
  		srv_diff_host_rate(row(30)),
  		dst_host_count(row(31)),
  		dst_host_srv_count(row(32)),
  		dst_host_same_srv_rate(row(33)),
  		dst_host_diff_srv_rate(row(34)),
  		dst_host_same_src_port_rate(row(35)),
  		dst_host_srv_diff_host_rate(row(36)),
  		dst_host_serror_rate(row(37)),
  		dst_host_srv_serror_rate(row(38)),
  		dst_host_rerror_rate(row(39)),
  		dst_host_srv_rerror_rate(row(40))
  	)
	}

	// labels
	val NUM_DOS = 10
	val NUM_R2L = 15
	val NUM_U2R = 8
	val NUM_PROBE = 6

  lazy val attack_type = DiscreteFeature(
  	"normal",

  	// DOS
		"back", 
		"land",
		"neptune",
		"pod",
		"smurf",
		"teardrop",
		"apache2",
		"processtable",
		"mailbomb",
		"udpstorm",

		// R2L
		"ftp_write", 
		"guess_passwd", 
		"imap", 
		"multihop", 
		"phf", 
		"spy",
		"warezclient",
		"warezmaster",
		"snmpgetattack",
		"snmpguess",
		"named",
		"sendmail",
		"sqlattack",
		"xlock",
		"xsnoop",

		// U2R
		"buffer_overflow", 
		"loadmodule", 
		"perl", 
		"rootkit",
		"xterm",
		"ps",
		"httptunnel",
		"worm",
		
		// probe
		"ipsweep", 
		"nmap", 
		"portsweep",
		"satan",
		"mscan",
		"saint"		
	)

  lazy val attack_category = DiscreteFeature("normal", "DOS", "R2L", "U2R", "probing")

  // maps specific attacks to 5 main categories (normal, DOS, R2L, U2R, probing)  
  def coarseLabel(label: Rep[Int]): Rep[Int] = {
  	if (label == 0) attack_category("normal")
  	else if (label < NUM_DOS+1) attack_category("DOS")
  	else if (label < NUM_R2L+1) attack_category("R2L")
  	else if (label < NUM_U2R+1) attack_category("U2R")
  	else attack_category("probing")
  }

  def test(testSet: Rep[TrainingSet[Double,Int]], classify: Rep[DenseVectorView[Double]] => Rep[Int], numSamples: Rep[Int]) = {
    // returns [TN, TP, FP, FN]
    sum(0, numSamples) { i =>      
      if (i > 0 && i % 10000 == 0) println("sample: " + i)
     
      val trueLabel = coarseLabel(testSet.labels.apply(i))
      val classifyLabel = classify(testSet(i))
      val predictedLabel = coarseLabel(classifyLabel)

      if (trueLabel == attack_category("normal") && trueLabel == predictedLabel) {
        DenseVector(1, 0, 0, 0)
      }
      else if (trueLabel == predictedLabel) {
        DenseVector(0, 1, 0, 0)
      }
      else if (trueLabel == attack_category("normal")) {
        DenseVector(0, 0, 1, 0)
      }
      else {
        DenseVector(0, 0, 0, 1)
      }
    }    
  }

  def readKDDCupData(path: Rep[String], selectedFeatures: Rep[IndexVector] = 0::NUM_FEATURES): Rep[TrainingSet[Double,Int]] = {
  	val lines = readMatrix[String](path, s => s, ",")
  	val data = lines mapRows { row => parseNetworkRow(row) } getCols(selectedFeatures)
	 	val labels = lines.getCol(lines.numCols-1).map(s => attack_type(s.slice(0,s.length-1)))
	 	TrainingSet(data, labels) 
	}
}
