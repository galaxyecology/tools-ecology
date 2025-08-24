<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template name="head" match="/">

<head>
    <meta charset="utf-8"/>
    <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
    <meta name="viewport" content="width=device-width, initial-scale=1"/>
    <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
    <meta name="description" content=""/>
    <meta name="author" content=""/>
	<style>
body { padding-top: 70px; }
footer {
  margin-top: 45px;
  padding: 35px 10 36px;
  border-top: 1px solid #e5e5e5;
  color: #666;
  display: flex;
}

footer p {
  margin-bottom: 0;
}
footer div {
  flex: 1;
}
footer .madeby {
  text-align: right;
}

.table-responsive {     max-height:300px;   }
</style>
    <title><xsl:value-of select="//dataset/title" /></title>

    <!-- Bootstrap core CSS -->
    <link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" rel="stylesheet"/>
    <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
    <!--[if lt IE 9]>
      <script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"></script>
      <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
    <![endif]-->
  </head>
  
  
</xsl:template>
</xsl:stylesheet>
