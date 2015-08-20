Manual Testing
--------------

The relevant parts of the API:
* Usage: http://doc.gitlab.com/ce/api/
* Merge requests: http://doc.gitlab.com/ce/api/merge_requests.html

Find the id of your project:
```
curl --header "PRIVATE-TOKEN: <TOKEN>" "https://git.gnu.io/api/v3/projects"
```

List all merge requests of the snowdrift-ci-test project:
```
% curl --header "PRIVATE-TOKEN: <TOKEN>" "https://git.gnu.io/api/v3/projects/71/merge_requests"
[{"id":115,"iid":1,"project_id":71,"title":"Add README.md","description":"","state":"opened","created_at":"2015-08-18T12:03:34.850Z","updated_at":"2015-08-18T12:03:45.349Z","target_branch":"master","source_branch":"readme","upvotes":0,"downvotes":0,"author":{"name":"Nikita Karetnikov","username":"nkaretnikov","id":23,"state":"active","avatar_url":"https://secure.gravatar.com/avatar/a36e885b9053ea39b5d1d9fab9cebbb2?s=40\u0026d=identicon"},"assignee":null,"source_project_id":71,"target_project_id":71,"labels":[],"milestone":null}]
```

Run snowdrift-ci and make it send reports to git.gnu.io:
```
% stack exec snowdrift-ci 8086 https://git.gnu.io <TOKEN> data/snowdrift_ci_test.commands &
```

Feed a dummy merge request hook output to snowdrift-ci, making it clone
snowdrift-ci-test from GitHub, test it, and send the report to git.gnu.io:
```
% curl -X POST -d @data/snowdrift_ci_test.json --header "Content-Type: application/json; charset=utf-8" http://localhost:8086
```

"POST requests return 201 Created if the resource is successfully created and
return the newly created resource as JSON" (see http://doc.gitlab.com/ce/api/).
