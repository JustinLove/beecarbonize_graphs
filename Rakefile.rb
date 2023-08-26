pub = FileList['public/*.html']
versioned = pub.pathmap('public/20230810/%f')
src = FileList['src/*']
js = FileList['js/*']

file 'js/ImportData.js' => src do
  sh "elm make src/ImportData.elm --output js/ImportData.js"
end

file 'generated/resources.dot' => js do
  sh "node js/import-data.js"
end

file 'public/beecarbonize_tech_tree.html' => 'beecarbonize.dot' do |t|
  sh "bash bin/build_tech_tree.sh"
end

file 'public/beecarbonize_event_chances.html' => 'beecarbonizeevents.dot' do |t|
  sh "bash bin/build_event_chances.sh"
end

file 'public/beecarbonize_tech_resources.html' => 'generated/resources.dot' do |t|
  sh "bash bin/build_tech_resources.sh"
end

file 'public/beecarbonize_event_resources.html' => 'generated/event_resources.dot' do |t|
  sh "bash bin/build_event_resources.sh"
end

pub.each do |f|
  v = f.pathmap('public/20230810/%f')
  file v => f do
    sh "cp #{f} #{v}"
  end
end

task :default => versioned
