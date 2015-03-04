(defproject ides-parser "0.1.0-SNAPSHOT"
  :description "Epigraphic Citation parser for IDEs"
  :url "http://ides.io"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :plugins [[org.apache.maven.wagon/wagon-ssh-external "2.6"]]
  :repositories [["papyri.info" "http://dev.papyri.info/maven/"]]
  :deploy-repositories [["snapshots" 
                         {:url "scp://libdc3-dev-01.oit.duke.edu/srv/data/papyri.info/pn/maven"
                          :creds :gpg}]]
  :main ides-parser.core
  :aot [ides-parser.core])

(cemerick.pomegranate.aether/register-wagon-factory!
 "scp" #(let [c (resolve 'org.apache.maven.wagon.providers.ssh.external.ScpExternalWagon)]
          (clojure.lang.Reflector/invokeConstructor c (into-array []))))
