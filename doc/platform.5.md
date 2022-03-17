% PLATFORM(5) User Manual
%
% January 2021

# NAME

Platform App configuration files

# SYNOPSIS

{App}/.platform/{Resource}.yaml

# DESCRIPTION

Applications deployed onto the Freckle Platform, describe their resources in
files under a **.platform** directory named **{Resource}.yaml**. When run, the
basename of the current working directory defines the **App** to Platform CLI.

All artifact names will be scoped by **App** and **Resource**, which should be
unique within Freckle.

# SCHEMA

**inputs** :: *Map\<Text, ?Text\>*\

> Inputs are a way to extend and/or set *Parameters* of the eventual
> CloudFormation Stack being deployed. See **environments.{name}** for a
> combined example.

**environments.{name}** :: *Map\<Text, Value>*\

> Set input values by environment. Given,
>
> ```
> inputs:
>   Bucket:
>   LogLevel: debug
>
> environments:
>   prod:
>     Bucket: prod-assets
>     LogLevel: info
> ```
>
> 1. The deployed Stack template will have *Bucket* and *LogLevel* added to its
>    *Parameters*, unless they exist in the template already. Added parameters
>    will always have *Type: String* and no *Default*.
>
> 2. The behavior of *platform deploy -e prod*, will be equivalent to:
>
>    ```
>    platform deploy -e prod \
>      --parameter Bucket=prod-assets \
>      --parameter LogLevel=info
>    ```
>
>    Unless explicit values are given on the command-line.
>
> 3. The behavior of *platform deploy -e x*, will be equivalent to:
>
>    ```
>    platform deploy -e x --parameter LogLevel=debug
>    ```
>
>    In other words, *LogLevel* will be set, but *Bucket* will be required.
>
>    Note that required values are only required on Stack creation. Deployments
>    to existing Stacks will use existing values, if not overriden in that
>    deploy.

**uses** :: *Text*\

> Base template to use.
>
> ```
> uses: web
> ```

**with** :: *Value*\

> Configuration of the base template. Values in this context can use
> CloudFormation features like **!Ref** and **!Sub**.

**platform** :: *Value*\

> Settings about Platform deployment itself.

**platform.stackName** :: *String*\

> Change the name used for the Stack. Default is built using *Environment*,
> *App*, and *Resource*.

**platform.stackTemplate** :: *String*\

> Change the path used for the temporary local Template file. Default is built
> using *Environment*, *App*, and *Resource*.

**platform.stackParameters** :: *String*\

> Change the path used for the temporary local Parameters file. Default is built
> using *Environment*, *App*, and *Resource*.

**platform.docker** :: *Value*\

> Change how Dockerized resources are handled.

**platform.docker.repository** :: *Text*\

> The Docker repository name to use. If the *Resource* name is the same as the
> *App*, *frontrow/\${App}* is used; otherwise, *frontrow/\${App}-${Resource}*.

**platform.docker.context** :: *Path*\

> The build context to use. If the *Resource* name is the same as the *App*, *.*
> is used; otherwise, *./${Resource}*.

**platform.docker.dockerfile** :: *Path*\

> Set the *\-\-file* argument to *docker build*. Default is to do nothing, which
> instructs Docker to use *./Dockerfile* relative to the context. Note that, if
> specified, this path is relative to the current directory, not the context.

**platform.assets** :: *Value*\

> Change how static assets are stored to S3 for deployment.

**platform.assets.bucket** :: *Text*\

> S3 Bucket to use. Default is our *freckle-platform-assets* bucket.

**platform.assets.prefix** :: *Text*\

> Prefix for assets. Default is built using *App* and *Resource*. Leading and
> trailing slashes are required and will be added if omitted.

**platform.assets.dist** :: *Path*\

> Directory within which to locate assets. If the *Resource* name is the same as
> the *App*, *.* is used; otherwise, *./${Resource}*.

# BASE TEMPLATES

## web

ECS Fargate task, Application Load Balancer, and DNS record.

### Inputs

To be specified in **inputs** and **environments.{name}**.

**AppDomain**\

> Required. The domain to create DNS for. A wildcard certificate must exist for
> this domain, unless **AppCertificateArn** is given.

**AppSubDomain**\

> Required. The sub-domain to create DNS for.

**AppHealthCheckPath**\

> Optional. Path to use for ALB HealthCheck. Default is */health-check*.

**AppUnhealthyThresholdCount**\

> Optional. Count of times a host must fail a health check to be considered
> unhealthy. Default is *\"2\"*. Note that all **inputs** must be Strings (and
> so may require quoting), even if the eventual CloudFormation template uses it
> as a different type.

**PagerDutyIntegrationId**\

> Optional. Integration Key from a CloudWatch Integration in PagerDuty. If
> present, the App's alarms will notify there via SNS.

**AppCertificateArn**, **DatadogAgentImage**, **DatadogApiKey**,
**Environment**, **App**, **AppImage**\

> These Parameters are present in the template, can be specified explicitly on
> deployments, but are typically handled for you automatically.

### Configuration

To be specified in **with**.

**service.clusterName** :: *Value*\

> Skip creating an ECS Cluster resource and use the one given by name. This also
> sets up the task to be **LaunchType: EC2**, since we assume the only use-case
> for this option is a non-Fargate App.

**service.cpuScaling.targetUtilization** :: *Value*\

> Utilization (%) to trigger scaling. Default **20**.

**service.cpuScaling.minCapacity** :: *Value*\

> Scaled-down capacity. Default **2**.

**service.cpuScaling.maxCapacity** :: *Value*\

> Scaled-up capacity. Default **4**.

**service.deployment.minPercent** :: *Value*\

> Minimum percentage of service size during a deployment. Default **100**.

**service.deployment.maxPercent** :: *Value*\

> Maximum percentage of service size during a deployment. Default **200**.
>
> The defaults of *100/200* perform a safe, up-then-down rolling deploy. If your
> service is a singleton, you should use *0/100* for these settings.

**container** :: **Container**\

> See *SHARED CONFIGURATION*.

**role** :: **Role**\

> See *SHARED CONFIGURATION*.

**executionRole** :: **Role**\

> Works the same as **role**, but applies to the Service's ExecutionRole and not
> TaskRole.

**alarms.p95ThresholdSeconds** :: *Value*\

> Threshold (seconds) for response time 95th percentile alarm, default is **1**.

**alarms.responseRate4XXThresholdPercent** :: *Value*\

> Threshold (0-100) for 4XX response rate alarm, default is **25**.

**alarms.responseRate5XXThresholdPercent** :: *Value*\

> Threshold (0-100) for 5XX response rate alarm, default is **0.15**.

## scheduled

ECS Fargate task, run on a Scheduled via CloudWatch Events.

### Inputs

To be specified in **inputs** and **environments.{name}**.

**ScheduleExpression**\

> Required. The expression for scheduling the task. Examples:
>
> ```
> ScheduleExpression: "rate(1 minute)"
>
> ScheduleExpression: "rate(2 hours)"
>
> ScheduleExpression: "cron(5,35 14 * * ? *)"
>
> ScheduleExpression: "cron(15 10 ? * 6L 2002-2005)"
> ```
>
> See AWS docs for more details.

**DatadogAgentImage**, **DatadogApiKey**, **Environment**, **App**,
**AppImage**\

> These Parameters are present in the template, can be specified explicitly on
> deployments, but are typically handled for you automatically.

### Configuration

To be specified in **with**.

**container** :: **Container**\

> See *SHARED CONFIGURATION*.

**role** :: **Role**\

> See *SHARED CONFIGURATION*.

## serverless

Lambda function, consuming from optional SNS topic or API gateway, resident in
our shared VPC.

### Inputs

To be specified in **inputs** and **environments.{name}**.

**SNSTopicArn**\

> Optional. The SNS topic for the function to consume events from.

**HttpPath**\

> Optional. The HTTP path at which the lambda can be invoked via API Gateway.
> This can be a path part (e.g. **my/cool/path**) or even a proxy, catch-all
> (e.g. **{proxy+}**). It must not begin with a slash. Including this will
> provision an API Gateway with associated resources.

**HttpMethod**\

> Optional, ignored unless **HttpPath** is specified, default to **ANY**. The HTTP
> method by which the **HttpPath** can be invoked.

**Environment**, **App**, **S3Bucket**, **S3Path**\

> These Parameters are present in the template, can be specified explicitly on
> deployments, but are typically handled for you automatically.

**KeepWarmScheduleExpression**\

> Optional. A schedule expression upon which to invoke the lambda function for
> purposes of keeping warm instances available. This is probably best applied
> to API lambdas (those with **HttpPath** specified). **rate(15 minutes)** tends
> to be a good go-to value. For more information see the [AWS docs](https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html).

### Configuration

To be specified in **with**.

**runtime** :: *Value*\

> Lambda function runtime environment, default **nodejs12.x**.

**handler** :: *Value*\

> Lambda function handler, default **index.handler**.

**memory** :: *Value*\

> Lambda function memory, in MB, default **128**.

**environment** :: *Map\<Text, Value\>*\

> Environment variables for the Lambda function, default **{}**.

**role** :: **Role**\

> See *SHARED CONFIGURATION*.

# SHARED CONFIGURATION

These are values that can be specified in multiple types of templates' *with*.

**container.cpu** :: *Value*\

> Cpu value for the App Tasks' primary Container, default **1024**.

**container.memory** :: *Value*\

> Memory value for the App Tasks' primary Container, default **2GB**.

**container.environment** :: *Map\<Text, Value\>*\

> Environment variables for the App Task's primary Container, default **{}**.

**container.secrets** :: *Map\<Text, Value\>*\

> Secrets for the App Task's primary Container, default **{}**. Values in this
> map can be objects (like **environment**) so you can use **!Ref** or **!Sub**.
>
> ```
> with:
>   secrets:
>     DD_API_KEY: !Sub '/${Environment}/dd-api-key'
> ```

**container.mounts** :: *List\<Mount\>*\

> Define volumes and mount them in the primary container.
>
> Examples:
>
> ```
> with:
>   container:
>     mounts:
>       # Mount an EFS Volume
>       - name: my_volume
>         path: /mnt/efs
>         from:
>           efs: "fs-xyz" # EFS FilesystemId
>
>       # Supports Ref, Sub, ImportValue, etc
>       - name: ...
>         path: ...
>         from:
>           efs: !Ref "EFSFileSystemId"
>
>       # Mount a host path at the same path in container (only works with
>       # non-Fargate ECS).
>       - name: docker_sock
>         path: /var/run/docker.sock
>
>       # Change the path between host and container
>       - name: ...
>         path: /some/path
>         from:
>           host: /another/path
>
>       # Mount as read-only
>       - name: ...
>         path: ...
>         from: ...
>         readOnly: true
> ```

**container.mounts.name** :: *\<Text\>*\

> A name for the volume (required).

**container.mounts.path** :: *\<Path\>*\

> In-container path for the mount (required.)

**container.mounts.from** :: *\<MountFrom\>*\

> Source for the volume. If not specified, behaves like *{host: \<path\>}*. If
> both an *efs* and *host* key are present, *efs* will be preferred.

**container.mounts.from.efs** :: *\<Value\>*\

> An EFS File System Id. This is a *Value* so that *!Ref*, *!Sub*, etc can be
> used.

**container.mounts.from.host** :: *\<Path\>*\

> The path on the EC2 instance to mount. Only works for non-Fargate (i.e. you're
> using *clusterName*).

**container.mounts.readOnly** :: *\<Boolean\>*\

> Mount as read-only. Default is *false*.

**role.polices** :: *List\<Arn|Inline\>*\

> Policies (**Arn**s for Managed Policies, or **name**/**document** for inline)
> for the App Task's Role, default **[]**.
>
> ```
> with:
>   role:
>     policies:
>       - arn:aws:iam::aws:policy/AmazonPollyFullAccess
>       - name: !Sub '${Environment}-${App}-task-policy'
>         document:
>           Statement:
>             - Effect: Allow
>               Action: ...
> ```

# EXAMPLES

Simple web application named "myapp"

**.platform/myapp.yaml**

```
inputs:
  LogLevel: debug

environments:
  prod:
    LogLevel: info

uses: web

with:
  container:
    environment:
      LOG_LEVEL: !Ref "LogLevel"
```

# AUTHOR

Freckle Engineering <freckle-engineering@renaissance.com>

# SEE ALSO

**platform(8)**
