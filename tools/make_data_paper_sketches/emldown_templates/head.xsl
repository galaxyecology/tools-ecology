<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template name="head" match="/">

<head>
    <title><xsl:value-of select="//dataset/title" /></title>
</head>
</xsl:template>
</xsl:stylesheet>
